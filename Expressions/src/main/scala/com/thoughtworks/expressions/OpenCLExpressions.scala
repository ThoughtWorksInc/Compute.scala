package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._

import scala.collection.mutable
import scala.collection.JavaConverters._

object OpenCLExpressions {

  trait OpenCLTerm {
    def name: String
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
      final case class Structure(name: String, unpacked: Seq[String]) extends Accessor {
        def packed: Fastring = fast"struct $name"
      }

      final case class Atom(name: String) extends Accessor {
        def packed: Fastring = fast"$name"

        def unpacked: Seq[String] = Seq(name)
      }
    }
    import Accessor._

    final case class Code(globalDeclarations: Fastring = Fastring.empty,
                          globalDefinitions: Fastring = Fastring.empty,
                          accessor: Accessor)

  }

  def generateOpenCLKernelSourceCode(functionName: String,
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
        fast"$typeName ${parameter.name}"
      }

      val (outputParameters, outputAssignments) = outputs.map { output =>
        val packedOutput = functionContext.get(output).packed
        val packedOutputType = functionContext.get(output.`type`).packed
        val outputName = output.name
        val outputParameter = fast"global $packedOutputType *$outputName"
        val outputAssignment = fast"$outputName[get_global_linear_id()] = packedOutput;\n"
        (outputParameter, outputAssignment)
      }.unzip

      fastraw"""
        kernel void $functionName(const ${parameterDeclarations.mkFastring(", ")}, ${outputParameters.mkFastring(", ")}) {
          ${localDefinitions.mkFastring}

          // TODO: polyfill for get_global_linear_id
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
trait OpenCLExpressions extends ValueExpressions with FreshNames {
  import OpenCLExpressions._

  protected trait TermApi extends OpenCLTerm with ExpressionApi with super.TermApi

  type Term <: (Expression with Any) with TermApi

  protected trait TypeApi extends super.TypeApi with OpenCLType { this: Type =>

    protected trait TypedTermApi extends TermApi with super.TypedTermApi {}

    /** @template */
    type TypedTerm <: (Term with Any) with TypedTermApi

    protected trait IdentifierApi extends TermApi { this: Identifier =>
      // TODO:

      def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        OpenCLTerm.Code(accessor = OpenCLTerm.Accessor.Packed(fast"$name", context.get(`type`).unpacked.length))
      }
    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

  }

  type Type <: (Expression with Any) with TypeApi

  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>

    protected trait LiteralApi extends super.LiteralApi {
      def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        OpenCLTerm.Code(accessor = OpenCLTerm.Accessor.Atom(fast"${operand0.toString}"))
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi
  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

}
