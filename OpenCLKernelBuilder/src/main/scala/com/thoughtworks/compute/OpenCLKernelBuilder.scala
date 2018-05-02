package com.thoughtworks.compute

import java.text.DecimalFormat

import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.compute.OpenCLKernelBuilder.ClTypeDefinition._
import com.thoughtworks.compute.Expressions.AllExpressions
import com.thoughtworks.compute.NDimensionalAffineTransform._
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, Factory5, Factory6, inject}

import scala.collection.mutable

object OpenCLKernelBuilder {
  private val smallNumberFormat = {
    val format = new DecimalFormat()
    format.setMinimumFractionDigits(0)
    format.setGroupingUsed(false)
    format
  }
  private val bigNumberFormat = {
    val format = new DecimalFormat()
    format.setGroupingUsed(false)
    format
  }
  private def toLiteral(value: Double): String = {
    val format = if (value > Int.MaxValue) {
      bigNumberFormat
    } else {
      smallNumberFormat
    }
    format.format(value)
  }

  type ClTermCode = String
  type ClTypeCode = String

  type ClTypeDefineHandler = ClTypeSymbol => Unit

  trait ClTypeDefinition extends Product {
    def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler)
  }

  object ClTypeDefinition {
    private val Noop: ClTypeDefineHandler = Function.const(())

    final case class TupleDefinition(element: ClTypeDefinition, length: Int) extends ClTypeDefinition {
      def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler) = {
        val elementTypeCode = globalContext.cachedSymbol(element).typeCode
        val tupleTypeCode = globalContext.freshName(raw"""${elementTypeCode}_tuple""")
        val typeDefineHandler: ClTypeDefineHandler = { typeSymbol =>
          globalContext.globalDefinitions += fastraw"""
            typedef struct ${typeSymbol.typeCode} {
              ${elementTypeCode} tuple_data[$length];
            } ${typeSymbol.typeCode};
          """
        }
        tupleTypeCode -> typeDefineHandler
      }
    }

    final case class ArrayDefinition(element: ClTypeDefinition, shape: Array[Int]) extends ClTypeDefinition {
      def define(globalContext: GlobalContext): (ClTypeCode, ClTypeDefineHandler) = {
        val elementTypeCode = globalContext.cachedSymbol(element).typeCode
        val arrayTypeCode = globalContext.freshName(raw"""${elementTypeCode}_array""")
        val typeDefineHandler: ClTypeDefineHandler = { typeSymbol =>
          val dimensions = for (size <- shape) yield fast"[$size]"
          globalContext.globalDefinitions += fast"typedef global const ${elementTypeCode} (* ${typeSymbol.typeCode})${dimensions.mkFastring};\n"
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
      val name = if (encodedPrefix.headOption.forall(_.isDigit)) {
        raw"""_${encodedPrefix}_${seed}"""
      } else {
        raw"""${encodedPrefix}_${seed}"""
      }
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
trait OpenCLKernelBuilder extends AllExpressions {
  val globalContext: GlobalContext
  import globalContext._

  val localDefinitions = mutable.Buffer.empty[Fastring]

  def generateKernelSourceCode(functionName: String,
                               totalDimensions: Int,
                               clDimensions: Int,
                               parameters: Seq[Term],
                               outputs: Seq[Term]): Fastring = {
    val parameterDeclarations = for (parameter <- parameters) yield {
      fast"${parameter.typeCode} const restrict ${parameter.termCode}"
    }

    val (outputParameters, outputAssignments) = outputs.map { output =>
      val outputTermCode = output.termCode
      val outputTypeCode = output.typeCode
      val outputId = freshName("output")
      val outputParameter = fast"global $outputTypeCode * const restrict $outputId"
      val outputAssignment = if (totalDimensions > 0) {
        def outputIndex(dimension: Int): Fastring = {
          if (dimension == 0) {
            fast"__global_id_0"
          } else {
            fast"(${outputIndex(dimension - 1)} * __global_size_$dimension + __global_id_$dimension)"
          }
        }
        val index = outputIndex(totalDimensions - 1)
        fast"$outputId[$index] = $outputTermCode;\n"
      } else {
        fast"*$outputId = $outputTermCode;\n"
      }
      (outputParameter, outputAssignment)

    }.unzip

    val (dimensionSizeParamters, dimensionDefinitions) = if (totalDimensions <= clDimensions) {
      val dimensionSizeParamters = Nil

      val dimensionDefinitions = (0 until totalDimensions).map { d =>
        fastraw"""
          const int __global_size_$d = get_global_size($d);
          const int __global_id_$d = get_global_id($d);
        """
      }.mkFastring

      Tuple2(dimensionSizeParamters, dimensionDefinitions)
    } else {
      val parameterDimensions = totalDimensions - clDimensions
      val dimensionSizeParamters = (1 to parameterDimensions).map { d =>
        fast"const int __global_size_$d"
      }

      def strideDefinitions(d: Int, stride: Fastring = fast"1"): Fastring = {
        if (d == 0) {
          fast"const int __stride_0 = $stride;"
        } else {
          fastraw"""
                const int __stride_$d = $stride;
                ${strideDefinitions(d - 1, fast"__stride_$d * __global_size_$d")}
              """
        }
      }
      val dimensionDefinitions = {
        fastraw"""
        ${strideDefinitions(parameterDimensions)}
        const int __global_size_0 = get_global_size(0) / __stride_0;
        const int __global_id_0 = get_global_id(0) / __stride_0;

        ${(for (d <- 1 to parameterDimensions) yield {
          fast"const int __global_id_$d = get_global_id(0) / __stride_$d % __global_size_$d;"
        }).mkFastring("\n")}
        
        ${(for (d <- 1 until clDimensions) yield {
          fastraw"""
            const int __global_size_${d + parameterDimensions} = get_global_size($d);
            const int __global_id_${d + parameterDimensions} = get_global_id($d);
          """
        }).mkFastring("\n")}
      """
      }
      Tuple2(dimensionSizeParamters, dimensionDefinitions)
    }
    val allParameters = dimensionSizeParamters.view ++ parameterDeclarations ++ outputParameters
    fastraw"""
      kernel void $functionName(${allParameters.mkFastring(", ")}) {
        $dimensionDefinitions
        ${localDefinitions.mkFastring}
        ${outputAssignments.mkFastring}
      }
    """
  }

  protected trait ClTerm extends super.TermApi { this: Term =>
    def termCode: ClTermCode
    def typeCode: ClTypeCode
  }

  type Term <: ClTerm

  protected trait CodeValues extends ClTerm { this: Term =>
    val termCode: ClTermCode
    val typeCode: ClTypeCode
  }

  protected trait ClValueType extends super.ValueTypeApi {

    def typeSymbol: ClTypeSymbol

    def term(code: ClTermCode): ThisTerm

  }

  type ValueType <: (Type with Any) with ClValueType

  protected trait ClFloatType extends super.FloatTypeApi with FloatExpressionApi with ClValueType {
    @inject def termFactory: Factory1[ClTermCode, ThisTerm]

    def term(code: ClTermCode) = termFactory.newInstance(code)

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
      termFactory.newInstance(floatString)
    }

    def parameter(id: Any): ThisTerm = {
      val termSymbol = freshName(id.toString)
      termFactory.newInstance(termSymbol)
    }

    def min(leftHandSide: FloatTerm, rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${leftHandSide.typeCode} $valueTermName = fmin(${leftHandSide.termCode}, ${rightHandSide.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }

    def max(leftHandSide: FloatTerm, rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${leftHandSide.typeCode} $valueTermName = fmax(${leftHandSide.termCode}, ${rightHandSide.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }

    def log(operand0: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${operand0.typeCode} $valueTermName = log(${operand0.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }

    def exp(operand0: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${operand0.typeCode} $valueTermName = exp(${operand0.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }

    def abs(operand0: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${operand0.typeCode} $valueTermName = fabs(${operand0.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }

    def sqrt(operand0: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${operand0.typeCode} $valueTermName = sqrt(${operand0.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }

    def tanh(operand0: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const ${operand0.typeCode} $valueTermName = tanh(${operand0.termCode});
      """
      float.termFactory.newInstance(valueTermName)
    }
  }

  type FloatType <: (ValueType with Any) with ClFloatType

  protected trait ArrayView[LocalElement <: ValueTerm] extends super.ArrayTermApi with CodeValues {
    this: ArrayTerm =>
    val elementType: LocalElement#ThisType

    def transform(matrix1: MatrixData): ThisTerm = {
      val newMatrix: MatrixData =
        NDimensionalAffineTransform.concatenate(matrix, matrix1, originalShape.length)
      arrayViewFactory
        .newInstance(elementType, newMatrix, originalPaddingCode, originalShape, termCode, typeCode)
        .asInstanceOf[ThisTerm]
    }

    val originalPaddingCode: ClTermCode

    val originalShape: Array[Int]

    val matrix: MatrixData

    def extract: Element = {

      val numberOfRows = originalShape.length
      if (numberOfRows == 0) {
        val termId = freshName("")
        localDefinitions += fastraw"""
          const ${elementType.typeSymbol.typeCode} $termId = *$termCode;
        """
        elementType.term(termId).asInstanceOf[Element]
      } else {
        val numberOfColumns = matrix.length / numberOfRows
        if (matrix.length != numberOfRows * numberOfColumns) {
          throw new IllegalStateException()
        }

        val (indices, indexDefinitions, bounds) = (for {
          y <- 0 until numberOfRows
        } yield {
          val products = for {
            x <- 0 until numberOfColumns
            scale = matrix(y * numberOfColumns + x)
            if scale != 0.0
          } yield {
            if (x < numberOfColumns - 1) {
              scale match {
                case 1.0 =>
                  fast"__global_id_$x"
                case scale =>
                  fast"__global_id_$x * ${toLiteral(scale)}"
              }
            } else {
              fast"${toLiteral(scale)}"
            }
          }
          val indexId = freshName("index")
          val (indexDefinition, bounds) = if (products.isEmpty) {
            (fast"const int $indexId = 0;\n", Nil)
          } else {
            (fast"const int $indexId = (int)(${products.mkFastring(" + ")});\n",
             Seq(fast"((bool)($indexId >= 0))", fast"((bool)($indexId < ${originalShape(y)}))"))
          }
          (indexId, indexDefinition, bounds)
        }).unzip3

        localDefinitions ++= indexDefinitions

        val termId = freshName("")
        val dereferenceCode = fast"(*${termCode})${indices.map { i =>
          fast"[$i]"
        }.mkFastring}"
        val checkedDereference = {
          val flatBounds = bounds.flatten
          if (flatBounds.isEmpty) {
            dereferenceCode
          } else {
            fast"(${bounds.flatten.mkFastring(" && ")}) ? $dereferenceCode : $originalPaddingCode"
          }
        }
        localDefinitions += fastraw"""
          const ${elementType.typeSymbol.typeCode} $termId = $checkedDereference;
        """
        elementType.term(termId).asInstanceOf[Element]
      }
    }
  }

  @inject
  protected def arrayViewFactory[LocalElement <: ValueTerm]
    : Factory6[LocalElement#ThisType,
               MatrixData,
               ClTermCode,
               Array[Int],
               ClTermCode,
               ClTypeCode,
               ArrayTerm with ArrayView[LocalElement] { type Element = LocalElement }]

  protected trait ArrayParameter[LocalElement <: ValueTerm] extends super.ArrayTermApi with CodeValues {
    thisArrayParameter: ArrayTerm =>

    val elementType: LocalElement#ThisType
    val paddingCode: ClTermCode
    val shape: Array[Int]

    def transform(matrix: MatrixData): ThisTerm = {
      arrayViewFactory.newInstance(elementType, matrix, paddingCode, shape, termCode, typeCode).asInstanceOf[ThisTerm]
    }

    def extract: Element = {
      val globalIndices = for {
        i <- shape.indices
      } yield fast"[__global_id_$i]"

      val bounds = for {
        (max, i) <- shape.view.zipWithIndex
      } yield fast"__global_id_$i < $max"

      val valueTermName = freshName("")
      val dereferenceCode = fast"(*${thisArrayParameter.termCode})${globalIndices.mkFastring}"
      localDefinitions += fastraw"""
        const ${elementType.typeSymbol.typeCode} $valueTermName = (${bounds.mkFastring(" && ")}) ? $dereferenceCode : $paddingCode;
      """

      elementType.term(valueTermName).asInstanceOf[Element]
    }
  }

  @inject
  protected def arrayParameterFactory[LocalElement <: ValueTerm]
    : Factory5[LocalElement#ThisType,
               ClTermCode,
               Array[Int],
               ClTermCode,
               ClTypeCode,
               ArrayTerm with ArrayParameter[LocalElement] { type Element = LocalElement }]

  protected trait ClArraySingleton extends super.ArraySingletonApi {

    def parameter[Element0 <: ValueTerm](id: Any, padding: Element0, shape: Array[Int]): ArrayTerm {
      type Element = Element0
    } = {
      val elementType = padding.valueType
      val arrayDefinition = ArrayDefinition(elementType.typeSymbol.firstDefinition, shape)
      val arrayTypeSymbol = cachedSymbol(arrayDefinition)
      val termCode = freshName(id.toString)
      arrayParameterFactory[Element0].newInstance(elementType,
                                                  padding.termCode,
                                                  shape,
                                                  termCode,
                                                  arrayTypeSymbol.typeCode)

    }
  }

  type ArraySingleton <: ClArraySingleton

  protected trait ArrayFill extends super.ArrayTermApi with ClTerm { this: ArrayTerm =>

    def termCode: ClTermCode = extract.termCode
    def typeCode: ClTypeCode = extract.typeCode
    def transform(matrix: MatrixData): ThisTerm = {
      this.asInstanceOf[ThisTerm]
    }

    val extract: Element
  }

  @inject
  protected def arrayFillFactory[LocalElement <: ValueTerm]
    : Factory1[LocalElement, ArrayTerm with ArrayFill { type Element = LocalElement }]

  protected trait ClValueTerm extends ElementTermApi with ClTerm { thisValue: ValueTerm =>

    val termCode: ClTermCode

    def valueType: ThisType

    def typeCode: ClTypeCode = valueType.typeSymbol.typeCode

    def fill: ArrayTerm { type Element = thisValue.ThisTerm } = {
      arrayFillFactory[thisValue.ThisTerm].newInstance(this.asInstanceOf[ThisTerm])
    }
  }
  type ValueTerm <: (Term with Any) with ClValueTerm

  protected trait ClFloatTerm extends super.FloatTermApi with ValueTermApi { this: FloatTerm =>

    def valueType: ThisType = float

    def unary_- : FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = -$termCode;
      """
      float.termFactory.newInstance(valueTermName)
    }

    def unary_+ : FloatTerm = {
      float.termFactory.newInstance(termCode)
    }

    def +(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode + ${rightHandSide.termCode};
      """
      float.termFactory.newInstance(valueTermName)
    }

    def -(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode - ${rightHandSide.termCode};
      """
      float.termFactory.newInstance(valueTermName)
    }

    def *(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode * ${rightHandSide.termCode};
      """
      float.termFactory.newInstance(valueTermName)
    }

    def /(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode / ${rightHandSide.termCode};
      """
      float.termFactory.newInstance(valueTermName)
    }

    def %(rightHandSide: FloatTerm): FloatTerm = {
      val valueTermName = freshName("")
      localDefinitions += fastraw"""
        const $typeCode $valueTermName = $termCode % ${rightHandSide.termCode};
      """
      float.termFactory.newInstance(valueTermName)
    }
  }
  type FloatTerm <: (ValueTerm with Any) with ClFloatTerm

  trait ClTupleTerm extends TupleTermApi with ClValueTerm { thisTupleTerm: TupleTerm =>
    def split: Seq[Element] = new IndexedSeq[Element] {

      def length: Int = thisTupleTerm.length

      def apply(index: Int): Element = {
        val elementTermName = freshName("")
        localDefinitions += fastraw"""
          const ${elementType.typeSymbol.typeCode} $elementTermName = $termCode.tuple_data[$index];
        """
        valueType.term(elementTermName).asInstanceOf[Element]
      }
    }

    def valueType: ThisType = clTupleTypeFactory[Element].newInstance(elementType, length).asInstanceOf[ThisType]

    val elementType: ValueType

    val length: Int

  }
  type TupleTerm <: (ValueTerm with Any) with ClTupleTerm

  @inject
  protected def tupleTermFactory[LocalElement <: ValueTerm]
    : Factory3[ValueType, Int, ClTermCode, TupleTerm { type Element = LocalElement }]

  protected trait ClTupleType extends TupleTypeApi with ClValueType { thisTupleType: TupleType =>
    def typeSymbol: ClTypeSymbol = {
      val tupleDefinition = TupleDefinition(elementType.typeSymbol.firstDefinition, length)
      cachedSymbol(tupleDefinition)
    }

    def term(code: ClTermCode): ThisTerm = {
      tupleTermFactory.newInstance(elementType, length, code)
    }

    val elementType: ValueType

    val length: Int
  }

  type TupleType <: (ValueType with Any) with ClTupleType

  @inject protected def clTupleTypeFactory[LocalElement <: ValueTerm]
    : Factory2[ValueType, Int, TupleType { type Element = LocalElement }]

  protected trait ClTupleSingleton extends TupleSingletonApi {

    def apply(element: ValueType, length: Int): TupleType { type Element = element.ThisTerm } = {
      clTupleTypeFactory[element.ThisTerm].newInstance(element, length)
    }

    def parameter(id: Any, element: ValueType, length: Int): TupleTerm { type Element = element.ThisTerm } = {
      val termCode = freshName(id.toString)
      tupleTermFactory[element.ThisTerm].newInstance(element, length, termCode)
    }

    def join[Element0 <: ValueTerm](elements: Element0*): TupleTerm {
      type Element = Element0
    } = {
      val elementType = elements.head.valueType

      val tupleType = clTupleTypeFactory[Element0].newInstance(elementType, elements.length)

      val tupleTermName = freshName("")
      localDefinitions += fastraw"""
        const ${tupleType.typeSymbol.typeCode} $tupleTermName = {{
          ${elements.map(_.termCode).mkFastring(""",
          """)}
        }};
      """

      tupleType
        .term(tupleTermName)
        .asInstanceOf[TupleTerm {
          type Element = Element0
        }]
    }
  }

  type TupleSingleton <: ClTupleSingleton

}
