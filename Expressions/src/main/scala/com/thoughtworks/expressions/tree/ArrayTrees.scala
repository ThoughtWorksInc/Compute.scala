package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Arrays
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayTrees extends Arrays with ValueTrees {

  type Category >: this.type <: Arrays

  type ArrayTree[LocalElement <: ValueTerm] = TreeApi {
    type ForeignTerm[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#ForeignTerm[C]
    }
  }

  @inject def arrayFactory[LocalElement <: ValueTerm]
    : Factory3[Array[Int],
               ArrayTree[LocalElement],
               Factory1[TreeApi {
                          type ForeignTerm[C <: Category] = LocalElement#ForeignTerm[C]
                        },
                        LocalElement],
               ArrayTerm {
                 type Element = LocalElement
               }]

  protected trait ArrayApi extends super.ArrayApi with TermApi { thisArray: ArrayTerm =>
    type ForeignTerm[C <: Category] = C#ArrayTerm {
      type Element = thisArray.Element#ForeignTerm[C]
    }
    val shape: Array[Int]

    val valueFactory: Factory1[TreeApi {
                                 type ForeignTerm[C <: Category] = thisArray.Element#ForeignTerm[C]
                               },
                               Element]

    def extract: Element = {
      valueFactory.newInstance(new TreeApi {
        def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
          thisArray.in(foreignCategory).extract
        }
        type ForeignTerm[C <: Category] = thisArray.Element#ForeignTerm[C]
      })
    }

    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      tree.in(foreignCategory)
    }
  }

  type ArrayTerm <: (Term with Any) with ArrayApi

  protected trait ValueApi extends super[Arrays].ValueApi with super[ValueTrees].ValueApi { thisValue: ValueTerm =>
    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.Self
    } = {
      arrayFactory.newInstance(
        shape.toArray,
        new TreeApi {
          def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
            val foreignValue: thisValue.ForeignTerm[foreignCategory.type] = thisValue.in(foreignCategory)
            foreignValue.fill(shape: _*).asInstanceOf[ForeignTerm[foreignCategory.type]]
          }

          type ForeignTerm[C <: Category] = C#ArrayTerm {
            type Element = thisValue.Self#ForeignTerm[C]
          }
        },
        thisValue.factory.asInstanceOf[Factory1[TreeApi {
                                                  type ForeignTerm[C <: Category] = thisValue.Self#ForeignTerm[C]
                                                },
                                                thisValue.Self]]
      )
    }

  }

  type ValueTerm <: (Term with Any) with ValueApi

}
