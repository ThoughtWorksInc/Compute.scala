package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._

import scala.collection.mutable
import scala.collection.JavaConverters._
import java.util.IdentityHashMap

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLExpressions extends Expressions {

  protected trait IdentifierApi extends DslExpressionApi { this: Identifier =>
    // TODO:
    def toCode(context: Context): DslExpression.Code = {
      DslExpression.Code(accessor = context.resolve(this))
    }
  }

  type Identifier <: (DslExpression with Any) with IdentifierApi

  trait DslEffect {

    def toCode(context: Context): DslEffect.Code

  }

  object DslEffect {

    type Statement = Fastring

    final case class Code(globalDeclarations: Fastring = Fastring.empty,
                          globalDefinitions: Fastring = Fastring.empty,
                          localDefinitions: Fastring = Fastring.empty,
                          statements: Fastring = Fastring.empty)

    final case class Update(buffer: DslExpression, index: DslExpression, value: DslExpression, valueType: DslType)
        extends DslEffect {
      override def toCode(context: Context): Code = {
        val valueName = context.freshName("update")
        Code(
          localDefinitions = fast"""
  ${context.get(valueType).packed} $valueName = ${context.get(value).packed};""",
          statements = fast"""
  ${context.get(buffer).packed}[${context.get(index).packed}] = $valueName;"""
        )
      }
    }

  }

  final case class Parameter(id: Identifier, dslType: DslType)

  final case class KernelDefinition(name: String, parameters: Seq[Parameter], effects: Seq[DslEffect])

  def generateSourceCode(kernels: KernelDefinition*): Fastring = {

    var seed = 0
    def nextId() = {
      val id = seed
      seed += 1
      id
    }

    val types = mutable.Set.empty[Seq[String]]
    val globalDeclarations = mutable.Buffer.empty[Fastring]
    val globalDefinitions = mutable.Buffer.empty[Fastring]
    val typeCodeCache = mutable.HashMap.empty[DslType, DslType.Accessor]

    val exportedFunctions = for {
      KernelDefinition(functionName, parameters, effects) <- kernels
    } yield {

      val parameterMap = mutable.Map.empty[Any, (String, DslType)]

      val localDefinitions = mutable.Buffer.empty[Fastring]

      val expressionCodeCache = new IdentityHashMap[DslExpression, DslExpression.Accessor]().asScala
      val effectCodeCache = new IdentityHashMap[DslEffect, Fastring]().asScala

      val functionContext = new Context {

        override def get(dslType: DslType): DslType.Accessor = {
          typeCodeCache.getOrElseUpdate(dslType, {
            val code = dslType.toCode(this)
            globalDeclarations += code.globalDeclarations
            globalDefinitions += code.globalDefinitions
            code.accessor

          })
        }
        override def get(expression: DslExpression): DslExpression.Accessor = {
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

        override def freshName(prefix: String): String = {
          raw"""${prefix}_${nextId()}"""
        }

        override def get(effect: DslEffect): Fastring = {
          effectCodeCache.getOrElseUpdate(
            effect, {
              val code = effect.toCode(this)
              localDefinitions += code.localDefinitions
              globalDeclarations += code.globalDeclarations
              globalDefinitions += code.globalDefinitions
              code.statements
            }
          )
        }

        override def resolve(id: Identifier) = {
          val (name, dslType) = parameterMap(id)
          DslExpression.Accessor.Packed(fast"$name", get(dslType).unpacked.length)
        }
      }

      val parameterDeclarations = for (parameter <- parameters) yield {
        val name = s"parameter_${nextId()}"
        parameterMap(parameter.id) = name -> parameter.dslType
        val typeName = functionContext.get(parameter.dslType).packed
        fast"$typeName $name"
      }

      val effectStatements = for (effect <- effects) yield {
        functionContext.get(effect)
      }

      fastraw"""
__kernel void $functionName(${parameterDeclarations.mkFastring(", ")}) {
  ${localDefinitions.mkFastring}
  ${effectStatements.mkFastring}
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
    def freshName(prefix: String): String

    def resolve(id: Identifier): DslExpression.Accessor

    def get(dslFunction: DslExpression): DslExpression.Accessor
    def get(dslType: DslType): DslType.Accessor
    def get(effect: DslEffect): DslEffect.Statement
  }

  protected type DslExpressionCompanion <: DslExpressionCompanionApi
  protected trait DslExpressionCompanionApi {

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

  protected trait DslExpressionApi extends super.DslExpressionApi {
    def toCode(context: Context): DslExpression.Code
  }

  type DslExpression <: DslExpressionApi

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

  protected trait DslTypeApi extends super.DslTypeApi {

    def toCode(context: Context): DslType.Code

    protected trait DslExpressionApi

    /** @template */
    type DslExpression <: OpenCLExpressions.this.DslExpression with DslExpressionApi

  }

  type DslType <: DslTypeApi

}
