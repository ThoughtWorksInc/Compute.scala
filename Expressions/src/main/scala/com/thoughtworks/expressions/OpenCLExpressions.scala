package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._

import scala.collection.mutable
import scala.collection.JavaConverters._
import java.util.IdentityHashMap
import java.util.concurrent.atomic.AtomicInteger

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLExpressions extends ValueExpressions with FreshNames {

//  trait DslEffect {
//
//    def toCode(context: Context): DslEffect.Code
//
//  }

//  object DslEffect {
//
//    type Statement = Fastring
//
//    final case class Code(globalDeclarations: Fastring = Fastring.empty,
//                          globalDefinitions: Fastring = Fastring.empty,
//                          localDefinitions: Fastring = Fastring.empty,
//                          statements: Fastring = Fastring.empty)
//
//    final case class Update(buffer: Term, index: Term, value: Term, valueType: DslType)
//        extends DslEffect {
//      override def toCode(context: Context): Code = {
//        val valueName = context.freshName("update")
//        Code(
//          localDefinitions = fast"""
//  ${context.get(valueType).packed} $valueName = ${context.get(value).packed};""",
//          statements = fast"""
//  ${context.get(buffer).packed}[${context.get(index).packed}] = $valueName;"""
//        )
//      }
//    }
//
//  }

  final case class ShaderDefinition(name: String, parameters: Seq[IdentifierApi], rhs: Term)

  def generateSourceCode(shaders: ShaderDefinition*): Fastring = {

    val globalDeclarations = mutable.Buffer.empty[Fastring]
    val globalDefinitions = mutable.Buffer.empty[Fastring]
    val typeCodeCache = mutable.HashMap.empty[DslType, DslType.Accessor]

    val exportedFunctions = for {
      ShaderDefinition(functionName, parameters, rhs) <- shaders
    } yield {

      val localDefinitions = mutable.Buffer.empty[Fastring]

      val expressionCodeCache = new IdentityHashMap[Term, Term.Accessor]().asScala
      val functionContext = new Context {

        override def get(dslType: DslType): DslType.Accessor = {
          typeCodeCache.getOrElseUpdate(dslType, {
            val code = dslType.toCode(this)
            globalDeclarations += code.globalDeclarations
            globalDefinitions += code.globalDefinitions
            code.accessor

          })
        }
        override def get(expression: Term): Term.Accessor = {
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
        val typeName = functionContext.get(parameter.dslType).packed
        fast"$typeName ${parameter.name}"
      }

      val output = functionContext.get(rhs).packed
      val outputType = functionContext.get(rhs.dslType).packed

      fastraw"""
        kernel void $functionName(${parameterDeclarations.mkFastring(", ")}, global $outputType *__output) {
          ${localDefinitions.mkFastring}

          // TODO: polyfill for get_global_linear_id
          __output[get_global_linear_id()] = $output;
        }
      """
    }
    fastraw"""
${globalDeclarations.mkFastring}
${globalDefinitions.mkFastring}
${exportedFunctions.mkFastring}
"""
  }

  trait Context {
    def get(dslFunction: Term): Term.Accessor
    def get(dslType: DslType): DslType.Accessor
  }

  protected type TermCompanion <: TermCompanionApi
  protected trait TermCompanionApi {

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
        override def packed: Fastring = value
        override def unpacked = Seq(value)
      }

      final case class Unpacked(unpacked: Seq[Fastring]) extends Accessor {
        override def packed: Fastring = unpacked match {
          case Seq(single) => single
          case _           => fast"{ ${unpacked.mkFastring(", ")} }"
        }
      }

      final case class Packed(packed: Fastring, numberOfFields: Int) extends Accessor {
        override def unpacked: Seq[Fastring] = {
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

  protected trait TermApi extends ExpressionApi with super.TermApi {
    def toCode(context: Context): Term.Code
  }

  type Term <: (Expression with Any) with TermApi

  protected trait DslTypeCompanionApi {

    trait Accessor {
      def packed: Fastring
      def unpacked: Seq[String]
    }

    object Accessor {
      final case class Structure(name: String, override val unpacked: Seq[String]) extends Accessor {
        override def packed: Fastring = fast"struct $name"
      }

      final case class Atom(name: String) extends Accessor {
        override def packed: Fastring = fast"$name"

        override def unpacked: Seq[String] = Seq(name)
      }
    }
    import Accessor._

    final case class Code(globalDeclarations: Fastring = Fastring.empty,
                          globalDefinitions: Fastring = Fastring.empty,
                          accessor: Accessor)

  }
  protected type DslTypeCompanion <: DslTypeCompanionApi

  protected trait DslTypeApi extends super.DslTypeApi { this: DslType =>

    def toCode(context: Context): DslType.Code

    protected trait TypedTermApi extends TermApi with super.TypedTermApi {}

    /** @template */
    type TypedTerm <: (Term with Any) with TypedTermApi

    type Identifier <: (TypedTerm with Any) with IdentifierApi

  }

  protected trait IdentifierApi extends TermApi { this: Term =>
    // TODO:

    def toCode(context: Context): Term.Code = {
      Term.Code(accessor = Term.Accessor.Packed(fast"$name", context.get(dslType).unpacked.length))
    }
  }

  type DslType <: (Expression with Any) with DslTypeApi

}
