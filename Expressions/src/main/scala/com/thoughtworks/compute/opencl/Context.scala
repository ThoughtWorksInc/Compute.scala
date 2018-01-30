package com.thoughtworks.compute.opencl

import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.compute.api.{Arrays, FloatArrays, Floats, Terms}
import com.thoughtworks.compute.opencl.Context.ClTypeDefinition.{ArrayDefinition, FloatDefinition}
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, Factory4, Factory5, Factory6, inject}
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}

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
        val elementTypeCode = globalContext.cachedSymbol(element).typeCode
        val arrayTypeCode = globalContext.freshName(raw"""${elementTypeCode}_array""")
        val typeDefineHandler: ClTypeDefineHandler = { typeSymbol =>
          val dimensions = for (size <- shape) yield fast"[$size]"
          globalContext.globalDefinitions += fast"typedef global ${elementTypeCode} (* ${typeSymbol.typeCode})${dimensions.mkFastring};"
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

  final case class ClTypeSymbol(firstDefinition: ClTypeDefinition, typeCode: ClTypeCode)

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
      val (name, defined) = typeDefinition.define(this)
      val typeSymbol = typeSymbolCache.getOrElseUpdate(typeDefinition, {
        ClTypeSymbol(typeDefinition, name)
      })
      if (typeSymbol.firstDefinition eq typeDefinition) {
        defined(typeSymbol)
      }
      typeSymbol
    }

  }

}
import com.thoughtworks.compute.opencl.Context._

/**
  * @author 杨博 (Yang Bo)
  */
trait Context extends FloatArrays {
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
    def termCode: ClTermCode
    def typeCode: ClTypeCode
  }

  type Term <: TermApi

  protected trait CodeValues extends TermApi { this: Term =>
    val termCode: ClTermCode
    val typeCode: ClTypeCode
  }

  protected trait ValueTypeApi extends super.ValueTypeApi {

    def typeSymbol: ClTypeSymbol

    @inject def factory: Factory2[ClTermCode, ClTypeCode, ThisTerm with CodeValues]

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait FloatTypeApi extends super.FloatTypeApi with FloatExpressionApi with ValueTypeApi {
    def typeSymbol: ClTypeSymbol = floatSymbol

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
        raw"""${value}f"""
      }
      factory.newInstance(floatString, float.typeSymbol.typeCode)
    }

    def parameter(id: Any): ThisTerm = {
      val termSymbol = freshName(id.toString)
      factory.newInstance(termSymbol, float.typeSymbol.typeCode)
    }
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

  protected trait ArrayView[LocalElement <: ValueTerm] extends super.ArrayTermApi with CodeValues {
    this: ArrayTerm =>
    val elementType: LocalElement#ThisType

    def transform(matrix1: RealMatrix): ThisTerm = {
      val originalShapeSize = originalShape.length
      val previousShapeSize = matrix.getRowDimension
      val newShapeSize = matrix1.getRowDimension
      assert(matrix.getColumnDimension == originalShapeSize + 1)
      assert(matrix.getRowDimension == previousShapeSize)
      assert(matrix1.getColumnDimension == previousShapeSize + 1)

      val newMatrix = MatrixUtils.createRealMatrix(newShapeSize, originalShapeSize + 1)

      for (newY <- 0 until newShapeSize) {
        for (newX <- 0 until originalShapeSize) {
          var accumulator = 0.0
          for (previousY <- 0 until previousShapeSize) {
            accumulator += matrix1.getEntry(newY, previousY) * matrix.getEntry(previousY, newX)
          }
          newMatrix.setEntry(newY, newX, accumulator)
        }
        locally {
          var accumulator = matrix1.getEntry(newY, previousShapeSize)
          for (previousY <- 0 until previousShapeSize) {
            accumulator += matrix1.getEntry(newY, previousY) * matrix.getEntry(previousY, originalShapeSize)
          }
          newMatrix.setEntry(newY, originalShapeSize, accumulator)
        }
      }

      arrayViewFactory
        .newInstance(elementType, newMatrix, originalPadding, originalShape, termCode, typeCode)
        .asInstanceOf[ThisTerm]
    }

    def translate(offset: Int*): ThisTerm = {
      val newMatrix = matrix.copy()
      val lastColumnIndex = newMatrix.getColumnDimension
      for (y <- 0 until newMatrix.getRowDimension) {
        newMatrix.addToEntry(y, lastColumnIndex, offset(y))
      }

      arrayViewFactory
        .newInstance(elementType, newMatrix, originalPadding, originalShape, termCode, typeCode)
        .asInstanceOf[ThisTerm]
    }
    val originalPadding: LocalElement#JvmValue

    val originalShape: Seq[Int]

    val matrix: RealMatrix

    def extract: Element = {
      val (indices, indexDefinitions) = (for {
        y <- 0 until matrix.getRowDimension
      } yield {
        val products = for {
          x <- 0 until matrix.getColumnDimension
          if matrix.getEntry(y, x) != 0.0
        } yield {
          if (x < originalShape.length) {
            // TODO: check boundary
            fast"get_global_id($x) * ${matrix.getEntry(y, x)}"
          } else {
            fast"${matrix.getEntry(y, x)}"
          }
        }
        val indexId = freshName("index")
        indexId -> fast"size_t $indexId = ${products.mkFastring(" + ")}"
      }).unzip

//      fast"

      val bounds = for {
        (max, indexId) <- originalShape.view.zip(indices)
      } yield fast"$indexId >= 0 && $indexId < $max"

      localDefinitions ++= indexDefinitions

      val termId = freshName("")
      val paddingCode = elementType.literal(originalPadding.asInstanceOf[elementType.JvmValue]).termCode
      val dereferenceCode = fast"(*${termCode})${indices.map { i =>
        fast"[$i]"
      }.mkFastring}"
      localDefinitions += fastraw"""
        const ${elementType.typeSymbol.typeCode} $termId = (${bounds.mkFastring(" && ")}) ? $paddingCode : $dereferenceCode;
      """
      elementType.factory.newInstance(termId, elementType.typeSymbol.typeCode).asInstanceOf[Element]
    }
  }

  @inject
  def arrayViewFactory[LocalElement <: ValueTerm]
    : Factory6[LocalElement#ThisType,
               RealMatrix,
               LocalElement#JvmValue,
               Seq[Int],
               ClTermCode,
               ClTypeCode,
               ArrayTerm with ArrayView[LocalElement] { type Element = LocalElement }]

  protected trait ArrayParameter[LocalElement <: ValueTerm] extends super.ArrayTermApi with CodeValues {
    thisArrayParameter: ArrayTerm =>

    val elementType: LocalElement#ThisType
    val padding: LocalElement#JvmValue
    val shape: Seq[Int]

    def transform(matrix: RealMatrix): ThisTerm = {
      if (matrix.getColumnDimension != shape.length) {
        throw new IllegalArgumentException
      }
      arrayViewFactory.newInstance(elementType, matrix, padding, shape, termCode, typeCode).asInstanceOf[ThisTerm]
    }
    def translate(offsets: Int*): ThisTerm = {
      if (offsets.length != shape.length) {
        throw new IllegalArgumentException
      }
      val matrix = MatrixUtils.createRealMatrix(shape.length, shape.length + 1)
      for (i <- shape.indices) {
        matrix.setEntry(i, i, 1.0)
        matrix.setEntry(i, shape.length, offsets(i))
      }
      arrayViewFactory.newInstance(elementType, matrix, padding, shape, termCode, typeCode).asInstanceOf[ThisTerm]
    }

    def extract: Element = {
      val globalIndices = for {
        i <- shape.indices
      } yield fast"[get_global_id($i)]"

      val bounds = for {
        (max, i) <- shape.view.zipWithIndex
      } yield fast"get_global_id($i) >= 0 && get_global_id($i) < $max"

      val valueTermName = freshName("")
      val paddingCode = elementType.literal(padding.asInstanceOf[elementType.JvmValue]).termCode
      val dereferenceCode = fast"(*${thisArrayParameter.termCode})${globalIndices.mkFastring}"
      localDefinitions += fastraw"""
        const ${elementType.typeSymbol.typeCode} $valueTermName = (${bounds.mkFastring(" && ")}) ? $dereferenceCode : $paddingCode;
      """

      elementType.factory.newInstance(valueTermName, elementType.typeSymbol.typeCode).asInstanceOf[Element]
    }
  }

  @inject
  def arrayParameterFactory[LocalElement <: ValueTerm]
    : Factory5[LocalElement#ThisType,
               LocalElement#JvmValue,
               Seq[Int],
               ClTermCode,
               ClTypeCode,
               ArrayTerm with ArrayParameter[LocalElement] { type Element = LocalElement }]

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    def parameter[Padding, ElementType <: ValueType { type JvmValue = Padding }](id: Any,
                                                                                 elementType: ElementType,
                                                                                 padding: Padding,
                                                                                 shape: Int*): ArrayTerm {
      type Element = elementType.ThisTerm
    } = {
      val arrayDefinition = ArrayDefinition(elementType.typeSymbol.firstDefinition, shape: _*)
      val arrayTypeSymbol = cachedSymbol(arrayDefinition)
      val termCode = freshName(id.toString)
      arrayParameterFactory[elementType.ThisTerm].newInstance(elementType.asInstanceOf[elementType.ThisTerm#ThisType],
                                                              padding.asInstanceOf[elementType.ThisTerm#JvmValue],
                                                              shape,
                                                              termCode,
                                                              arrayTypeSymbol.typeCode)
    }
  }

  type ArrayCompanion <: ArrayCompanionApi

  protected trait ArrayFill extends super.ArrayTermApi with TermApi { this: ArrayTerm =>

    def termCode: ClTermCode = extract.termCode
    def typeCode: ClTypeCode = extract.typeCode
    def transform(matrix: RealMatrix): ThisTerm = {
      this.asInstanceOf[ThisTerm]
    }

    def translate(offset: Int*): ThisTerm = {
      this.asInstanceOf[ThisTerm]
    }

    val extract: Element
  }

  @inject
  def arrayFillFactory[LocalElement <: ValueTerm]
    : Factory1[LocalElement, ArrayTerm with ArrayFill { type Element = LocalElement }]

  protected trait ValueTermApi extends super.ValueTermApi { thisValue: ValueTerm =>
    def fill: ArrayTerm { type Element = thisValue.ThisTerm } = {
      arrayFillFactory[thisValue.ThisTerm].newInstance(this.asInstanceOf[ThisTerm])
    }
  }
  type ValueTerm <: (Term with Any) with ValueTermApi

}
