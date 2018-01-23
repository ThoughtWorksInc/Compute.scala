package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Arrays
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

  final case class Extract[LocalElement <: ValueTerm](array: ArrayTree[LocalElement]) extends TreeApi {
    def export(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      array.export(foreignCategory).extract
    }
    type ForeignTerm[C <: Category] = LocalElement#ForeignTerm[C]

  }

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
      valueFactory.newInstance(Extract(tree))
    }

  }

  type ArrayTerm <: (Term with Any) with ArrayApi

  final case class Fill[LocalElement <: ValueTerm](element: TreeApi {
    type ForeignTerm[C <: Category] = LocalElement#ForeignTerm[C]
  }, shape: Int*)
      extends TreeApi {
    type ForeignTerm[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#ForeignTerm[C]
    }

    def export(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      element.export(foreignCategory).fill(shape: _*).asInstanceOf[ForeignTerm[foreignCategory.type]]
    }
  }

  protected trait ValueApi extends super[Arrays].ValueApi with super[ValueTrees].ValueApi with TermApi {
    thisValue: ValueTerm =>

    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.Self
    } = {
      val fillTree = Fill[thisValue.Self](
        tree.asInstanceOf[TreeApi { type ForeignTerm[C <: Category] = thisValue.Self#ForeignTerm[C] }],
        shape: _*)
      arrayFactory[Self].newInstance(
        shape.toArray,
        fillTree,
        thisValue.factory
          .asInstanceOf[Factory1[TreeApi {
                                   type ForeignTerm[C <: Category] = thisValue.Self#ForeignTerm[C]
                                 },
                                 thisValue.Self]]
      )
    }

  }

  type ValueTerm <: (Term with Any) with ValueApi

}
