package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Floats
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, inject}
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatTrees extends Floats with ValueTrees {

  protected trait FloatTree extends FloatApi with ValueTree { thisFloat: FloatTerm =>
    type ForeignTerm[C <: Category] = C#FloatTerm

    protected val exporter: Exporter
    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      exporter.in(foreignCategory)
    }

    def factory: Factory1[ExporterApi {
                            type ForeignTerm[C <: Category] = C#FloatTerm
                          },
                          Self] = {
      floatFactory
    }
  }

  override type FloatTerm <: (ValueTerm with Any) with FloatTree

  @inject
  def floatFactory: Factory1[ExporterApi {
                               type ForeignTerm[C <: Category] = C#FloatTerm
                             },
                             FloatTerm]

}
