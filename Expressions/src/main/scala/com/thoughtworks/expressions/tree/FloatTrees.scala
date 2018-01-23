package com.thoughtworks.expressions.tree

import java.util.IdentityHashMap

import com.thoughtworks.expressions.api.Floats
import com.thoughtworks.feature.Factory.{Factory1, inject}
import scala.collection.JavaConverters._
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatTrees extends Floats with ValueTrees {

  protected trait FloatExpressionApi extends ValueExpressionApi {
    type ForeignTerm[C <: Category] = C#FloatTerm
  }

  protected trait FloatApi extends super.FloatApi with ValueApi with FloatExpressionApi { thisFloat: FloatTerm =>

    def factory: Factory1[TreeApi {
                            type ForeignTerm[C <: Category] = C#FloatTerm
                          },
                          TypedTerm] = {
      floatFactory
    }
  }

  override type FloatTerm <: (ValueTerm with Any) with FloatApi

  @inject
  def floatFactory: Factory1[TreeApi {
                               type ForeignTerm[C <: Category] = C#FloatTerm
                             },
                             FloatTerm]

  final case class FloatParameter(id: Any) extends TreeApi {
    type ForeignTerm[C <: Category] = C#FloatTerm

    def export(foreignCategory: Category,
               map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): foreignCategory.FloatTerm = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.FloatTerm.parameter(id))
        .asInstanceOf[foreignCategory.FloatTerm]
    }

  }

  final case class FloatLiteral(value: Float) extends TreeApi {
    type ForeignTerm[C <: Category] = C#FloatTerm

    def export(foreignCategory: Category,
               map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): foreignCategory.FloatTerm = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.FloatTerm.literal(value))
        .asInstanceOf[foreignCategory.FloatTerm]
    }
  }

  protected trait FloatTypeApi extends super.FloatTypeApi with FloatExpressionApi {
    def literal(value: Float): FloatTerm = {
      floatFactory.newInstance(FloatLiteral(value))
    }

    def parameter(id: Any): FloatTerm = {
      floatFactory.newInstance(FloatParameter(id))
    }

    def factory: Factory1[TreeApi {
                            type ForeignTerm[C <: Category] = TypedTerm#ForeignTerm[C]
                          },
                          TypedTerm] = floatFactory
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
