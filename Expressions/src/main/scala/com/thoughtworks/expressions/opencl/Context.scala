package com.thoughtworks.expressions.opencl

import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.expressions.api.{Arrays, FloatArrays, Floats, Terms}
import com.thoughtworks.expressions.opencl.Context.TypeDefinition.{ArrayDefinition, FloatDefinition}
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

import scala.collection.mutable
object Context {
  type TermName = String
  type TypeName = String

  type TypeDefineHandler = TypeSymbol => Unit

  trait TypeDefinition extends Product {
    def makeTypeName(globalContext: GlobalContext): (TypeName, TypeDefineHandler)
  }

  object TypeDefinition {
    final case class ArrayDefinition(element: TypeDefinition, shape: Int*) extends TypeDefinition {
      def makeTypeName(globalContext: GlobalContext): (TypeName, TypeDefineHandler) = {
        val elementName = globalContext.typeSymbol(element).name
        val arrayName = globalContext.freshName(raw"""${elementName}_array""")
        val typeDefineHandler: TypeDefineHandler = { typeSymbol =>
          val dimensions = for (size <- shape) yield fast"[$size]"
          globalContext.globalDefinitions += fast"typedef global ${elementName} (* ${typeSymbol.name})${dimensions.mkFastring};"
        }
        arrayName -> typeDefineHandler
      }
    }

    final case object FloatDefinition extends TypeDefinition {
      def makeTypeName(globalContext: GlobalContext) = {
        val dummy = { typeSymbol: TypeSymbol =>
          ()
        }
        "float" -> dummy
      }
    }
  }

  final case class TypeSymbol(definition: TypeDefinition, name: TypeName)

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
    private val typeCache = mutable.HashMap.empty[TypeDefinition, TypeSymbol]

    val floatSymbol = typeSymbol(FloatDefinition)

    def typeSymbol(typeDefinition: TypeDefinition): TypeSymbol = {
      val (name, define) = typeDefinition.makeTypeName(this)
      val typeSymbol = typeCache.getOrElseUpdate(typeDefinition, TypeSymbol(typeDefinition, name))
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

  trait TermSymbol {
    protected def makeTermName(): TermName
    val name: TermName = makeTermName()
  }
  //  def resolve(id: Any): DslExpression.Accessor
  //
  //  def get(dslFunction: DslExpression): DslExpression.Accessor
  //  def get(dslType: DslType): DslType.Accessor
  //  def get(effect: DslEffect): DslEffect.Statement

  def generateKernelSourceCode(functionName: String,
                               numberOfDimensions: Int,
                               parameters: Seq[Term],
                               outputs: Seq[Term]): Fastring = {
    val parameterDeclarations = for (parameter <- parameters) yield {
      fast"const ${parameter.typeSymbol.name} ${parameter.termSymbol.name}"
    }

    val (outputParameters, outputAssignments) = outputs.map { output =>
      val outputTermName = output.termSymbol.name
      val outputTypeName = output.typeSymbol.name
      val outputParameter = fast"global $outputTypeName *output_$outputTermName"
      def outputIndex(dimension: Int): Fastring = {
        if (dimension == 0) {
          fast"get_global_id(0)"
        } else {
          fast"(${outputIndex(dimension - 1)} * get_global_size($dimension) + get_global_id($dimension))"
        }
      }

      val index = outputIndex(numberOfDimensions - 1)
      val outputAssignment = fast"output_$outputTermName[$index] = $outputTermName;\n"
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
    val termSymbol: TermSymbol
    val typeSymbol: TypeSymbol
  }

  type Term <: TermApi

  protected trait ValueTypeApi extends super.ValueTypeApi {

    def typeSymbol: TypeSymbol

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait FloatExpressionApi extends super.FloatExpressionApi with ValueExpressionApi {
    type TermIn[C <: Category] = C#FloatTerm
    type TypeIn[C <: Category] = C#FloatType
  }
  protected trait FloatTypeApi extends super.FloatTypeApi with FloatExpressionApi {
    def typeSymbol: TypeSymbol = floatSymbol

    @inject
    def factory: Factory2[TermSymbol, TypeSymbol, ThisTerm]

    def literal(value: Float): ThisTerm = {
      val termSymbol = new TermSymbol {
        protected def makeTermName(): TermName = {
          if (value.isNaN) {
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
        }
      }
      factory.newInstance(termSymbol, floatSymbol)
    }

    def parameter(id: Any): ThisTerm = {
      val termSymbol = new TermSymbol {
        protected def makeTermName(): TermName = freshName(id.toString)
      }
      factory.newInstance(termSymbol, floatSymbol)
    }
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

  protected trait ArrayApi extends super.ArrayApi with TermApi { this: ArrayTerm =>
    def extract: Element = ???
  }

  type ArrayTerm <: (Term with Any) with ArrayApi

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    @inject def factory[LocalElement <: ValueTerm]
      : Factory3[Array[Int], TermSymbol, TypeSymbol, ArrayTerm { type Element = LocalElement }]

    def parameter(id: Any, elementType: ValueType, shape: Int*): ArrayTerm { type Element = elementType.ThisTerm } = {
      val arrayDefinition = ArrayDefinition(elementType.typeSymbol.definition, shape: _*)
      val arrayTypeSymbol = typeSymbol(arrayDefinition)
      factory[elementType.ThisTerm].newInstance(shape.toArray, new TermSymbol {
        protected def makeTermName(): TermName = {
          freshName(id.toString)
        }
      }, arrayTypeSymbol)
    }

  }

  type ArrayCompanion <: ArrayCompanionApi

  protected trait ValueApi extends super.ValueApi { thisValue: ValueTerm =>
    def fill(shape: Int*): ArrayTerm { type Element = thisValue.ThisTerm } = {
      ???
    }
  }
  type ValueTerm <: (Term with Any) with ValueApi

}
