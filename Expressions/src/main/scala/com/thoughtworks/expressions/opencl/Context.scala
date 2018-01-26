package com.thoughtworks.expressions.opencl

import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.expressions.api.{Arrays, FloatArrays, Floats, Terms}
import com.thoughtworks.expressions.opencl.Context.ClTypeDefinition.{ArrayDefinition, FloatDefinition}
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

import scala.collection.mutable
object Context {

  type ClTermCode = String
  type ClTypeCode = String

  type ClTypeDefineHandler = ClTypeSymbol => Unit

  trait ClTypeDefinition extends Product {
    def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler)
  }

  object ClTypeDefinition {
    private val Noop: ClTypeDefineHandler = Function.const(())

    final case class ArrayDefinition(element: ClTypeDefinition, shape: Int*) extends ClTypeDefinition {
      def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler) = {
        val elementTypeCode = globalContext.cachedSymbol(element).code
        val arrayTypeCode = globalContext.freshName(raw"""${elementTypeCode}_array""")
        val typeDefineHandler: ClTypeDefineHandler = { typeSymbol =>
          val dimensions = for (size <- shape) yield fast"[$size]"
          globalContext.globalDefinitions += fast"typedef global ${elementTypeCode} (* ${typeSymbol.code})${dimensions.mkFastring};"
        }
        arrayTypeCode -> typeDefineHandler
      }
    }

    final case object FloatDefinition extends ClTypeDefinition {
      def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler) = {
        "float" -> Noop
      }
    }
  }

  final case class ClTypeSymbol(firstDefinition: ClTypeDefinition, code: ClTypeCode)

  final class GlobalContext {

    private var seed = 0

    def freshName(prefix: String): String = {
      val encodedPrefix = prefix.map {
        case c if c.isLetterOrDigit => c
        case _                      => '_'
      }
      val name = raw"""${encodedPrefix}_${seed}"""
      seed += 1
      name
    }

    val globalDeclarations = mutable.Buffer.empty[Fastring]
    val globalDefinitions = mutable.Buffer.empty[Fastring]
    private val typeSymbolCache = mutable.HashMap.empty[ClTypeDefinition, ClTypeSymbol]

    val floatSymbol = cachedSymbol(FloatDefinition)

    def cachedSymbol(typeDefinition: ClTypeDefinition): ClTypeSymbol = {
      val (name, define) = typeDefinition.define(this)
      val typeSymbol = typeSymbolCache.getOrElseUpdate(typeDefinition, ClTypeSymbol(typeDefinition, name))
      define(typeSymbol)
      typeSymbol
    }

  }

}
import com.thoughtworks.expressions.opencl.Context._

/**
  * @author 杨博 (Yang Bo)
  */
trait Context extends Terms with FloatArrays {
  protected val globalContext: GlobalContext
  import globalContext._

  val localDefinitions = mutable.Buffer.empty[Fastring]

  def generateKernelSourceCode(functionName: String,
                               numberOfDimensions: Int,
                               parameters: Seq[Term],
                               outputs: Seq[Term]): Fastring = {
    val parameterDeclarations = for (parameter <- parameters) yield {
      fast"const ${parameter.typeCode} ${parameter.termCode}"
    }

    val (outputParameters, outputAssignments) = outputs.map { output =>
      val outputTermCode = output.termCode
      val outputTypeCode = output.typeCode
      val outputParameter = fast"global $outputTypeCode *output_$outputTermCode"
      def outputIndex(dimension: Int): Fastring = {
        if (dimension == 0) {
          fast"get_global_id(0)"
        } else {
          fast"(${outputIndex(dimension - 1)} * get_global_size($dimension) + get_global_id($dimension))"
        }
      }

      val index = outputIndex(numberOfDimensions - 1)
      val outputAssignment = fast"output_$outputTermCode[$index] = $outputTermCode;\n"
      (outputParameter, outputAssignment)
    }.unzip
    fastraw"""
      kernel void $functionName(${parameterDeclarations.mkFastring(", ")}, ${outputParameters.mkFastring(", ")}) {
        ${localDefinitions.mkFastring}
        ${outputAssignments.mkFastring}
      }
    """
  }

  protected trait TermApi extends super.TermApi { this: Term =>
    val termCode: ClTermCode
    val typeCode: ClTypeCode
  }

  type Term <: TermApi

  protected trait ValueTypeApi extends super.ValueTypeApi {

    def typeSymbol: ClTypeSymbol

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait FloatExpressionApi extends super.FloatExpressionApi with ValueExpressionApi {
    type TermIn[C <: Category] = C#FloatTerm
    type TypeIn[C <: Category] = C#FloatType
  }
  protected trait FloatTypeApi extends super.FloatTypeApi with FloatExpressionApi {
    def typeSymbol: ClTypeSymbol = floatSymbol

    @inject
    def factory: Factory2[ClTermCode, ClTypeCode, ThisTerm]

    def literal(value: Float): ThisTerm = {
      val floatString = if (value.isNaN) {
        "NAN"
      } else if (value.isInfinite) {
        if (value > 0) {
          "INFINITE"
        } else {
          "(-INFINITE)"
        }
      } else {
        value.toString
      }
      factory.newInstance(floatString, float.typeSymbol.code)
    }

    def parameter(id: Any): ThisTerm = {
      val termSymbol = freshName(id.toString)
      factory.newInstance(termSymbol, float.typeSymbol.code)
    }
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

  protected trait ArrayTermApi extends super.ArrayTermApi with TermApi { this: ArrayTerm =>
    def extract: Element = {
      ???
    }
  }

  type ArrayTerm <: (Term with Any) with ArrayTermApi

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    @inject def factory[LocalElement <: ValueTerm]
      : Factory3[Array[Int], ClTermCode, ClTypeCode, ArrayTerm { type Element = LocalElement }]

    def parameter(id: Any, elementType: ValueType, shape: Int*): ArrayTerm { type Element = elementType.ThisTerm } = {
      val arrayDefinition = ArrayDefinition(elementType.typeSymbol.firstDefinition, shape: _*)
      val arrayTypeSymbol = cachedSymbol(arrayDefinition)
      val termCode = freshName(id.toString)
      factory[elementType.ThisTerm].newInstance(shape.toArray, termCode, arrayTypeSymbol.code)
    }
  }

  type ArrayCompanion <: ArrayCompanionApi

  protected trait ValueTermApi extends super.ValueTermApi { thisValue: ValueTerm =>
    def fill(shape: Int*): ArrayTerm { type Element = thisValue.ThisTerm } = {
      array.factory[thisValue.ThisTerm].newInstance(shape.toArray, termCode, typeCode)
    }
  }
  type ValueTerm <: (Term with Any) with ValueTermApi

}
