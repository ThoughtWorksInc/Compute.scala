package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Floats
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, inject}
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatTrees extends Floats with ValueTrees {

  protected trait FloatApi extends super.FloatApi with ValueApi { thisFloat: FloatTerm =>
    type ForeignTerm[C <: Category] = C#FloatTerm

    def factory: Factory1[TreeApi {
                            type ForeignTerm[C <: Category] = C#FloatTerm
                          },
                          Self] = {
      floatFactory
    }
  }

  override type FloatTerm <: (ValueTerm with Any) with FloatApi

  @inject
  def floatFactory: Factory1[TreeApi {
                               type ForeignTerm[C <: Category] = C#FloatTerm
                             },
                             FloatTerm]

  final case class FloatLiteral(value: Float) extends TreeApi {
    type ForeignTerm[C <: Category] = C#FloatTerm

    def export(foreignCategory: Category): foreignCategory.FloatTerm = {
      foreignCategory.FloatTerm.literal(value)
    }
  }

  protected trait FloatCompanionApi extends super.FloatCompanionApi {
    def literal(value: Float): FloatTerm =
      floatFactory.newInstance(FloatLiteral(value))
  }

  type FloatCompanion <: FloatCompanionApi

}
