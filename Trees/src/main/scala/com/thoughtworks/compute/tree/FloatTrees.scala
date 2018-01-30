package com.thoughtworks.compute.tree

import com.thoughtworks.compute.Expressions.Floats
import com.thoughtworks.feature.Factory.{Factory1, inject}
import scala.collection.JavaConverters._
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatTrees extends Floats with ValueTrees {

  protected trait FloatTermApi extends super.FloatTermApi with ValueTermApi with FloatExpressionApi {
    thisFloat: FloatTerm =>

    def factory: Factory1[TreeApi { type TermIn[C <: Category] = C#FloatTerm }, ThisTerm] = {
      float.factory
    }
  }

  type FloatTerm <: (ValueTerm with Any) with FloatTermApi

  final case class FloatParameter(id: Any) extends TreeApi with Parameter { thisParameter =>
    type TermIn[C <: Category] = C#FloatTerm

    def export(foreignCategory: Category, map: ExportContext): foreignCategory.FloatTerm = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.float.parameter(id))
        .asInstanceOf[foreignCategory.FloatTerm]
    }

    def alphaConversion(context: AlphaConversionContext): TreeApi = {
      def converted = {
        val newId = new AnyRef {
          override val toString: String = raw"""α-converted(${thisParameter.toString})"""
        }
        FloatParameter(newId)
      }
      context.asScala.getOrElseUpdate(this, converted)
    }

  }

  final case class FloatLiteral(value: Float) extends TreeApi with Operator {

    def alphaConversion(context: AlphaConversionContext): TreeApi = this

    type TermIn[C <: Category] = C#FloatTerm

    def export(foreignCategory: Category, map: ExportContext): foreignCategory.FloatTerm = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.float.literal(value))
        .asInstanceOf[foreignCategory.FloatTerm]
    }
  }

  protected trait FloatTypeApi extends super.FloatTypeApi with FloatExpressionApi {
    def in(foreignCategory: Category): TypeIn[foreignCategory.type] = {
      foreignCategory.float
    }

    def literal(value: Float): FloatTerm = {
      factory.newInstance(FloatLiteral(value))
    }

    def parameter(id: Any): FloatTerm = {
      factory.newInstance(FloatParameter(id))
    }

    @inject
    def factory: Factory1[TreeApi { type TermIn[C <: Category] = ThisTerm#TermIn[C] }, ThisTerm]
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
