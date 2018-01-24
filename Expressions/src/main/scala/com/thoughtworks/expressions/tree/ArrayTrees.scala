package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Arrays
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}
import scala.collection.JavaConverters._
import java.util.IdentityHashMap

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayTrees extends Arrays with ValueTrees {

  type ArrayTree[LocalElement <: ValueTerm] = TreeApi {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#TermIn[C]
    }
  }

  @inject def arrayFactory[LocalElement <: ValueTerm]
    : Factory3[Array[Int],
               ArrayTree[LocalElement],
               Factory1[TreeApi {
                          type TermIn[C <: Category] = LocalElement#TermIn[C]
                        },
                        LocalElement],
               ArrayTerm {
                 type Element = LocalElement
               }]

  final case class Extract[LocalElement <: ValueTerm](array: ArrayTree[LocalElement]) extends TreeApi {
    def export(foreignCategory: Category,
               map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, array.export(foreignCategory, map).extract)
        .asInstanceOf[TermIn[foreignCategory.type]]
    }
    type TermIn[C <: Category] = LocalElement#TermIn[C]

  }

  protected trait ArrayApi extends super.ArrayApi with TermApi { thisArray: ArrayTerm =>
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = thisArray.Element#TermIn[C]
    }

    val shape: Array[Int]

    val valueFactory: Factory1[TreeApi {
                                 type TermIn[C <: Category] = thisArray.Element#TermIn[C]
                               },
                               Element]

    def extract: Element = {
      valueFactory.newInstance(Extract(tree))
    }

  }

  type ArrayTerm <: (Term with Any) with ArrayApi

  final case class Fill[LocalElement <: ValueTerm](element: TreeApi {
    type TermIn[C <: Category] = LocalElement#TermIn[C]
  }, shape: Int*)
      extends TreeApi {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#TermIn[C]
    }

    def export(foreignCategory: Category,
               map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, element.export(foreignCategory, map).fill(shape: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]
    }
  }

  protected trait ValueApi extends super[Arrays].ValueApi with super[ValueTrees].ValueApi with TermApi {
    thisValue: ValueTerm =>

    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.ThisTerm
    } = {
      val fillTree = Fill[thisValue.ThisTerm](
        tree.asInstanceOf[TreeApi { type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C] }],
        shape: _*)
      arrayFactory[ThisTerm].newInstance(
        shape.toArray,
        fillTree,
        thisValue.factory
          .asInstanceOf[Factory1[TreeApi {
                                   type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C]
                                 },
                                 thisValue.ThisTerm]]
      )
    }

  }

  type ValueTerm <: (Term with Any) with ValueApi

  final case class ArrayParameter[ElementType <: ValueType](id: Any, elementType: ValueType, shape: Int*)
      extends TreeApi {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = elementType.TermIn[C]
    }

    def export(foreignCategory: Category,
               map: IdentityHashMap[TreeApi, Any] = new IdentityHashMap[TreeApi, Any]): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.array.parameter(id, elementType.in(foreignCategory), shape: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]

    }
  }

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    def parameter(id: Any, elementType: ValueType, shape: Int*): ArrayTerm {
      type Element = elementType.ThisTerm
    } = {
      val parameterTree = ArrayParameter[elementType.type](id, elementType, shape: _*)
      arrayFactory[elementType.ThisTerm].newInstance(
        shape.toArray,
        parameterTree.asInstanceOf[ArrayTree[elementType.ThisTerm]],
        elementType.factory
      )
    }

  }

  type ArrayCompanion <: ArrayCompanionApi
}
