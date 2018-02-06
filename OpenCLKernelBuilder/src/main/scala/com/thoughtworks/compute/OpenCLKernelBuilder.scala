package com.thoughtworks.compute

import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.compute.OpenCLKernelBuilder.ClTypeDefinition._
import com.thoughtworks.compute.Expressions.FloatArrays
import com.thoughtworks.compute.NDimensionalAffineTransform._
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory5, Factory6, inject}

import scala.collection.mutable
object OpenCLKernelBuilder {
//
//  private def transformMatrix(matrix: RealMatrix, matrix1: RealMatrix): RealMatrix = {
//    val originalShapeSize = matrix.getColumnDimension - 1
//    val previousShapeSize = matrix.getRowDimension
//    val newShapeSize = matrix1.getRowDimension
//    assert(matrix.getRowDimension == previousShapeSize)
//    assert(matrix1.getColumnDimension == previousShapeSize + 1)
//
//    val newMatrix = MatrixUtils.createRealMatrix(newShapeSize, originalShapeSize + 1)
//
//    for (newY <- 0 until newShapeSize) {
//      for (newX <- 0 until originalShapeSize) {
//        var accumulator = 0.0
//        for (previousY <- 0 until previousShapeSize) {
//          accumulator += matrix1.getEntry(newY, previousY) * matrix.getEntry(previousY, newX)
//        }
//        newMatrix.setEntry(newY, newX, accumulator)
//      }
//      locally {
//        var accumulator = matrix1.getEntry(newY, previousShapeSize)
//        for (previousY <- 0 until previousShapeSize) {
//          accumulator += matrix1.getEntry(newY, previousY) * matrix.getEntry(previousY, originalShapeSize)
//        }
//        newMatrix.setEntry(newY, originalShapeSize, accumulator)
//      }
//    }
//    newMatrix
//  }

  type ClTermCode = String
  type ClTypeCode = String

  type ClTypeDefineHandler = ClTypeSymbol => Unit

  trait ClTypeDefinition extends Product {
    def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler)
  }

  object ClTypeDefinition {
    private val Noop: ClTypeDefineHandler = Function.const(())

    final case class ArrayDefinition(element: ClTypeDefinition, shape: Array[Int]) extends ClTypeDefinition {
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

  final class GlobalContext extends Fastring {

    private var seed = 0

    def freshName(prefix: String): String = {
      val encodedPrefix = prefix.map {
        case c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') => c
        case _                                                                               => '_'
      }
      val name = raw"""${encodedPrefix}_${seed}"""
      seed += 1
      name
    }

    protected[OpenCLKernelBuilder] val globalDeclarations = mutable.Buffer.empty[Fastring]
    protected[OpenCLKernelBuilder] val globalDefinitions = mutable.Buffer.empty[Fastring]
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

    def foreach[U](f: String => U): Unit = {
      globalDeclarations.foreach(_.foreach(f))
      globalDefinitions.foreach(_.foreach(f))
    }
  }

}
import com.thoughtworks.compute.OpenCLKernelBuilder._

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLKernelBuilder extends FloatArrays {
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
      val outputId = freshName("output")
      val outputParameter = fast"global $outputTypeCode *$outputId"
      def outputIndex(dimension: Int): Fastring = {
        if (dimension == 0) {
          fast"get_global_id(0)"
        } else {
          fast"(${outputIndex(dimension - 1)} * get_global_size($dimension) + get_global_id($dimension))"
        }
      }

      val index = outputIndex(numberOfDimensions - 1)
      val outputAssignment = fast"$outputId[$index] = $outputTermCode;\n"
      (outputParameter, outputAssignment)
    }.unzip
    fastraw"""
      kernel void $functionName(${(parameterDeclarations.view ++ outputParameters).mkFastring(", ")}) {
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

    @inject def factory: Factory1[ClTermCode, ThisTerm]

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
      factory.newInstance(floatString)
    }

    def parameter(id: Any): ThisTerm = {
      val termSymbol = freshName(id.toString)
      factory.newInstance(termSymbol)
    }
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

  protected trait ArrayView[LocalElement <: ValueTerm] extends super.ArrayTermApi with CodeValues {
    this: ArrayTerm =>
    val elementType: LocalElement#ThisType

    def transform(matrix1: MatrixData): ThisTerm = {
      val newMatrix: MatrixData =
        NDimensionalAffineTransform.concatenate(matrix, matrix1, originalShape.length)
      arrayViewFactory
        .newInstance(elementType, newMatrix, originalPadding, originalShape, termCode, typeCode)
        .asInstanceOf[ThisTerm]
    }

    val originalPadding: LocalElement#JvmValue

    val originalShape: Array[Int]

    val matrix: MatrixData

    def extract: Element = {
      val numberOfRows = originalShape.length
      val numberOfColumns = matrix.length / numberOfRows

      val (indices, indexDefinitions) = (for {
        y <- 0 until numberOfRows
      } yield {
        val products = for {
          x <- 0 until numberOfColumns
          if matrix(y * numberOfColumns + x) != 0.0
        } yield {
          if (x < originalShape.length) {
            matrix(y * numberOfColumns + x) match {
              case 1.0 =>
                fast"get_global_id($x)"
              case scale =>
                fast"get_global_id($x) * $scale"
            }
          } else {
            fast"${matrix(y * numberOfColumns + x)}"
          }
        }
        val indexId = freshName("index")
        indexId -> fast"size_t $indexId = ${products.mkFastring(" + ")};\n"
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
        const ${elementType.typeSymbol.typeCode} $termId = (${bounds.mkFastring(" && ")}) ? $dereferenceCode : $paddingCode;
      """
      elementType.factory.newInstance(termId).asInstanceOf[Element]
    }
  }

  @inject
  def arrayViewFactory[LocalElement <: ValueTerm]
    : Factory6[LocalElement#ThisType,
               MatrixData,
               LocalElement#JvmValue,
               Array[Int],
               ClTermCode,
               ClTypeCode,
               ArrayTerm with ArrayView[LocalElement] { type Element = LocalElement }]

  protected trait ArrayParameter[LocalElement <: ValueTerm] extends super.ArrayTermApi with CodeValues {
    thisArrayParameter: ArrayTerm =>

    val elementType: LocalElement#ThisType
    val padding: LocalElement#JvmValue
    val shape: Array[Int]

    def transform(matrix: MatrixData): ThisTerm = {
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

      elementType.factory.newInstance(valueTermName).asInstanceOf[Element]
    }
  }

  @inject
  def arrayParameterFactory[LocalElement <: ValueTerm]
    : Factory5[LocalElement#ThisType,
               LocalElement#JvmValue,
               Array[Int],
               ClTermCode,
               ClTypeCode,
               ArrayTerm with ArrayParameter[LocalElement] { type Element = LocalElement }]

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    def parameter[Padding, ElementType <: ValueType { type JvmValue = Padding }](id: Any,
                                                                                 elementType: ElementType,
                                                                                 padding: Padding,
                                                                                 shape: Array[Int]): ArrayTerm {
      type Element = elementType.ThisTerm
    } = {
      val arrayDefinition = ArrayDefinition(elementType.typeSymbol.firstDefinition, shape)
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
    def transform(matrix: MatrixData): ThisTerm = {
      this.asInstanceOf[ThisTerm]
    }

    val extract: Element
  }

  @inject
  def arrayFillFactory[LocalElement <: ValueTerm]
    : Factory1[LocalElement, ArrayTerm with ArrayFill { type Element = LocalElement }]

  protected trait ValueTermApi extends super.ValueTermApi with TermApi { thisValue: ValueTerm =>

    val termCode: ClTermCode

    def fill: ArrayTerm { type Element = thisValue.ThisTerm } = {
      arrayFillFactory[thisValue.ThisTerm].newInstance(this.asInstanceOf[ThisTerm])
    }
  }
  type ValueTerm <: (Term with Any) with ValueTermApi

  protected trait FloatTermApi extends super.FloatTermApi with ValueTermApi { this: FloatTerm =>
    def typeCode: ClTypeCode = floatSymbol.typeCode

    def unary_- : FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = -$termCode;
      """
      float.factory.newInstance(valueTermName)
    }

    def unary_+ : FloatTerm = {
      float.factory.newInstance(termCode)
    }

    def +(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode + ${rightHandSide.termCode};
      """
      float.factory.newInstance(valueTermName)
    }

    def -(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode - ${rightHandSide.termCode};
      """
      float.factory.newInstance(valueTermName)
    }

    def *(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode * ${rightHandSide.termCode};
      """
      float.factory.newInstance(valueTermName)
    }

    def /(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode / ${rightHandSide.termCode};
      """
      float.factory.newInstance(valueTermName)
    }

    def %(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode % ${rightHandSide.termCode};
      """
      float.factory.newInstance(valueTermName)
    }
  }
  type FloatTerm <: (ValueTerm with Any) with FloatTermApi

}
