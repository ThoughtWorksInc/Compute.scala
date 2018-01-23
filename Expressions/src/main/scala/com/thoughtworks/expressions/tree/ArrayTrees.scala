package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Arrays
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}
import scala.collection.JavaConverters._
import java.util.IdentityHashMap

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
    def export(
        foreignCategory: Category,
        map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): ForeignTerm[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, array.export(foreignCategory, map).extract)
        .asInstanceOf[ForeignTerm[foreignCategory.type]]
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

    def export(
        foreignCategory: Category,
        map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): ForeignTerm[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, element.export(foreignCategory, map).fill(shape: _*))
        .asInstanceOf[ForeignTerm[foreignCategory.type]]
    }
  }

  protected trait ValueApi extends super[Arrays].ValueApi with super[ValueTrees].ValueApi with TermApi {
    thisValue: ValueTerm =>

    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.TypedTerm
    } = {
      val fillTree = Fill[thisValue.TypedTerm](
        tree.asInstanceOf[TreeApi { type ForeignTerm[C <: Category] = thisValue.TypedTerm#ForeignTerm[C] }],
        shape: _*)
      arrayFactory[TypedTerm].newInstance(
        shape.toArray,
        fillTree,
        thisValue.factory
          .asInstanceOf[Factory1[TreeApi {
                                   type ForeignTerm[C <: Category] = thisValue.TypedTerm#ForeignTerm[C]
                                 },
                                 thisValue.TypedTerm]]
      )
    }

  }

  type ValueTerm <: (Term with Any) with ValueApi

  final case class ArrayParameter[LocalElement <: ValueTerm](id: Any, elementType: ValueType {
    type TypedTerm = LocalElement
  }, shape: Int*)
      extends TreeApi {
    type ForeignTerm[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#ForeignTerm[C]
    }

    def export(
        foreignCategory: Category,
        map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): ForeignTerm[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.ArrayTerm.parameter(id, ???, shape: _*))
        .asInstanceOf[ForeignTerm[foreignCategory.type]]

    }
  }

  protected trait ArrayTypeApi extends super.ArrayTypeApi {

    def parameter[LocalElement <: ValueTerm](id: Any, elementType: ValueType {
      type TypedTerm = LocalElement
    }, shape: Int*): ArrayTerm {
      type Element = LocalElement
    } = {
//      val parameterTree = ArrayParameter[LocalElement](id, elementType, shape: _*)
//      arrayFactory[LocalElement].newInstance(
//        shape.toArray,
//        parameterTree,
//        elementType.factory
//      )
      ???
    }

  }

  type ArrayType <: ArrayTypeApi
}
