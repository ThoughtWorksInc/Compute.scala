package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Arrays
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayTrees extends Arrays with ValueTrees {

  type Category >: this.type <: Arrays

  type ArrayExporter[LocalElement <: ValueTerm] = ExporterApi {
    type ForeignTerm[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#ForeignTerm[C]
    }
  }

  @inject def arrayFactory[LocalElement <: ValueTerm]
    : Factory3[ArrayExporter[LocalElement],
               Array[Int],
               Factory1[ExporterApi {
                          type ForeignTerm[C <: Category] = LocalElement#ForeignTerm[C]
                        },
                        LocalElement],
               ArrayTerm {
                 type Element = LocalElement
               }]

  protected trait ArrayTree extends ArrayApi with Tree { thisArray: ArrayTerm =>
    type ForeignTerm[C <: Category] = C#ArrayTerm {
      type Element = thisArray.Element#ForeignTerm[C]
    }
    val shape: Array[Int]

    val valueFactory: Factory1[ExporterApi {
                                 type ForeignTerm[C <: Category] = thisArray.Element#ForeignTerm[C]
                               },
                               Element]

    def extract: Element = {
      valueFactory.newInstance(new ExporterApi {
        def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
          thisArray.in(foreignCategory).extract
        }
        type ForeignTerm[C <: Category] = thisArray.Element#ForeignTerm[C]
      })
    }

    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      exporter.in(foreignCategory)
    }
  }

  type ArrayTerm <: (Term with Any) with ArrayTree

  protected trait ValueTree extends super.ValueTree with ValueApi { thisValue: ValueTerm =>
    def filled(shape: Int*): ArrayTerm {
      type Element = thisValue.Self
    } = {
      arrayFactory.newInstance(
        new ExporterApi {
          def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
            val foreignValue: thisValue.ForeignTerm[foreignCategory.type] = thisValue.in(foreignCategory)
            foreignValue.filled(shape: _*).asInstanceOf[ForeignTerm[foreignCategory.type]]
          }

          type ForeignTerm[C <: Category] = C#ArrayTerm {
            type Element = thisValue.Self#ForeignTerm[C]
          }
        },
        shape.toArray,
        thisValue.factory.asInstanceOf[Factory1[ExporterApi {
                                                  type ForeignTerm[C <: Category] = thisValue.Self#ForeignTerm[C]
                                                },
                                                thisValue.Self]]
      )
    }

  }

  type ValueTerm <: (Term with Any) with ValueTree

}
