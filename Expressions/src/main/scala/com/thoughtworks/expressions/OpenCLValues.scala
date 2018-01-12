package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._

import scala.collection.mutable
import scala.collection.JavaConverters._

object OpenCLValues {

  trait OpenCLTerm {
    def id: String
    val `type`: OpenCLType
    def toCode(context: OpenCLContext): OpenCLTerm.Code
  }

  trait OpenCLContext {
    def get(term: OpenCLTerm): OpenCLTerm.Accessor
    def get(`type`: OpenCLType): OpenCLType.Accessor
  }

  object OpenCLTerm {

    final case class Code(globalDeclarations: Fastring = Fastring.empty,
                          globalDefinitions: Fastring = Fastring.empty,
                          localDefinitions: Fastring = Fastring.empty,
                          accessor: Accessor)
    trait Accessor {
      def unpacked: Seq[Fastring]
      def packed: Fastring
    }

    object Accessor {

      final case class Atom(value: Fastring) extends Accessor {
        def packed: Fastring = value
        def unpacked = Seq(value)
      }

      final case class Unpacked(unpacked: Seq[Fastring]) extends Accessor {
        def packed: Fastring = unpacked match {
          case Seq(single) => single
          case _           => fast"{ ${unpacked.mkFastring(", ")} }"
        }
      }

      final case class Packed(packed: Fastring, numberOfFields: Int) extends Accessor {
        def unpacked: Seq[Fastring] = {
          if (numberOfFields == 1) {
            Seq(packed)
          } else {
            for (i <- 0 until numberOfFields) yield {
              fast"$packed._$i"
            }
          }
        }
      }
    }
  }

  trait OpenCLType {
    def toCode(context: OpenCLContext): OpenCLType.Code
  }

  object OpenCLType {

    trait Accessor {
      def packed: Fastring

      // TODO: remove unpacked.
      // unpacked is designed to support weak type check.
      // We don't need it as we have strong type system.
      def unpacked: Seq[String]
    }

    object Accessor {
      final case class Structure(id: String, unpacked: Seq[String]) extends Accessor {
        def packed: Fastring = fast"struct $id"
      }

      final case class Atom(id: String) extends Accessor {
        def packed: Fastring = fast"$id"

        def unpacked: Seq[String] = Seq(id)
      }
    }
    import Accessor._

    final case class Code(globalDeclarations: Fastring = Fastring.empty,
                          globalDefinitions: Fastring = Fastring.empty,
                          accessor: Accessor)

  }

  def generateOpenCLKernelSourceCode(functionName: String,
                                     numberOfDimensions: Int,
                                     parameters: Seq[OpenCLTerm],
                                     outputs: Seq[OpenCLTerm]): Fastring = {

    val globalDeclarations = mutable.Buffer.empty[Fastring]
    val globalDefinitions = mutable.Buffer.empty[Fastring]
    val typeCodeCache = mutable.HashMap.empty[OpenCLType, OpenCLType.Accessor]

    val exportedFunction = {

      val localDefinitions = mutable.Buffer.empty[Fastring]

      val expressionCodeCache = new java.util.IdentityHashMap[OpenCLTerm, OpenCLTerm.Accessor]().asScala
      val functionContext = new OpenCLContext {

        def get(`type`: OpenCLType): OpenCLType.Accessor = {
          typeCodeCache.getOrElseUpdate(`type`, {
            val code = `type`.toCode(this)
            globalDeclarations += code.globalDeclarations
            globalDefinitions += code.globalDefinitions
            code.accessor
          })
        }

        def get(expression: OpenCLTerm): OpenCLTerm.Accessor = {
          expressionCodeCache.getOrElseUpdate(
            expression, {
              val code = expression.toCode(this)
              localDefinitions += code.localDefinitions
              globalDeclarations += code.globalDeclarations
              globalDefinitions += code.globalDefinitions
              code.accessor
            }
          )
        }

      }

      val parameterDeclarations = for (parameter <- parameters) yield {
        val typeName = functionContext.get(parameter.`type`).packed
        fast"const $typeName ${parameter.id}"
      }

      val (outputParameters, outputAssignments) = outputs.map { output =>
        val packedOutput = functionContext.get(output).packed
        val packedOutputType = functionContext.get(output.`type`).packed
        val outputId = output.id
        val outputParameter = fast"global $packedOutputType *output_$outputId"
        def outputIndex(dimension: Int): Fastring = {
          if (dimension == 0) {
            fast"get_global_id(0)"
          } else {
            fast"(${outputIndex(dimension - 1)} * get_global_size($dimension) + get_global_id($dimension))"
          }
        }

        val index = outputIndex(numberOfDimensions - 1)
        val outputAssignment = fast"output_$outputId[$index] = $packedOutput;\n"
        (outputParameter, outputAssignment)
      }.unzip

      fastraw"""
        kernel void $functionName(${parameterDeclarations.mkFastring(", ")}, ${outputParameters.mkFastring(", ")}) {
          ${localDefinitions.mkFastring}
          ${outputAssignments.mkFastring}
        }
      """
    }
    fastraw"""
${globalDeclarations.mkFastring}
${globalDefinitions.mkFastring}
${exportedFunction}
"""
  }

}

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLValues extends Values with FreshNames {
  import OpenCLValues._

  protected trait TermApi extends OpenCLTerm with super[FreshNames].ExpressionApi with super.TermApi

  type Term <: (Expression with Any) with TermApi

  protected trait TypeApi extends super.TypeApi with OpenCLType { this: Type =>

    protected trait TypedTermApi extends TermApi with super.TypedTermApi { this: TypedTerm =>

    }

    /** @template */
    type TypedTerm <: (Term with Any) with TypedTermApi

    protected trait IdentifierApi extends TermApi { this: Identifier =>
      def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        OpenCLTerm.Code(accessor = OpenCLTerm.Accessor.Packed(fast"$id", context.get(`type`).unpacked.length))
      }
    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

  }

  type Type <: (Expression with Any) with TypeApi

  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>

    protected trait LiteralApi extends super.LiteralApi { this: Literal =>
      def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        OpenCLTerm.Code(accessor = OpenCLTerm.Accessor.Atom(fast"${operand0.toString}"))
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi
  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

}
