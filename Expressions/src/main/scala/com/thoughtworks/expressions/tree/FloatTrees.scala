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

    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      tree.in(foreignCategory)
    }

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

  protected trait FloatCompanionApi extends super.FloatCompanionApi {
    def literal(value: Float): FloatTerm =
      floatFactory.newInstance(new TreeApi {
        type ForeignTerm[C <: Category] = C#FloatTerm

        def in(foreignCategory: Category): foreignCategory.FloatTerm = {
          foreignCategory.FloatTerm.literal(value)
        }
      })
  }

  type FloatCompanion <: FloatCompanionApi

}
