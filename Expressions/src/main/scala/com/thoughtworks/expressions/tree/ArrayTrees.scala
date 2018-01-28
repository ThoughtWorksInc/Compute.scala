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

  final case class Extract[LocalElement <: ValueTerm](array: ArrayTree[LocalElement]) extends TreeApi with Operator {
    def export(foreignCategory: Category, map: ExportMap): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, array.export(foreignCategory, map).extract)
        .asInstanceOf[TermIn[foreignCategory.type]]
    }
    type TermIn[C <: Category] = LocalElement#TermIn[C]

  }

  final case class Translate[LocalElement <: ValueTerm](array: ArrayTree[LocalElement], offset: Int*)
      extends TreeApi
      with Operator {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#TermIn[C]
    }

    def export(foreignCategory: Category, map: ExportMap): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, array.export(foreignCategory, map).translate(offset: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]
    }
  }

  protected trait ArrayTermApi extends super.ArrayTermApi with TermApi { thisArray: ArrayTerm =>
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = thisArray.Element#TermIn[C]
    }

    val valueFactory: Factory1[TreeApi {
                                 type TermIn[C <: Category] = thisArray.Element#TermIn[C]
                               },
                               Element]

    def extract: Element = {
      valueFactory.newInstance(Extract(tree))
    }

    override def translate(offset: Int*): ThisTerm = {
      val translatedTree = Translate[Element](tree, offset: _*)
      array
        .factory[Element]
        .newInstance(
          shape,
          translatedTree,
          valueFactory
        )
        .asInstanceOf[ThisTerm]
    }

  }

  type ArrayTerm <: (Term with Any) with ArrayTermApi

  final case class Fill[LocalElement <: ValueTerm](element: TreeApi {
    type TermIn[C <: Category] = LocalElement#TermIn[C]
  }, shape: Int*)
      extends TreeApi
      with Operator {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#TermIn[C]
    }

    def export(foreignCategory: Category, map: IdentityHashMap[TreeApi, Any]): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, element.export(foreignCategory, map).fill(shape: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]
    }
  }

  protected trait ValueTermApi extends super[Arrays].ValueTermApi with super[ValueTrees].ValueTermApi with TermApi {
    thisValue: ValueTerm =>

    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.ThisTerm
    } = {
      val fillTree = Fill[thisValue.ThisTerm](
        tree.asInstanceOf[TreeApi { type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C] }],
        shape: _*)
      array
        .factory[ThisTerm]
        .newInstance(
          shape,
          fillTree,
          thisValue.factory
            .asInstanceOf[Factory1[TreeApi {
                                     type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C]
                                   },
                                   thisValue.ThisTerm]]
        )
    }

  }

  type ValueTerm <: (Term with Any) with ValueTermApi

  final case class ArrayParameter[ElementType <: ValueType](id: Any, elementType: ValueType, shape: Int*)
      extends TreeApi
      with Parameter {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = elementType.TermIn[C]
    }

    def export(foreignCategory: Category, map: IdentityHashMap[TreeApi, Any]): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.array.parameter(id, elementType.in(foreignCategory), shape: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]

    }
  }

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    @inject def factory[LocalElement <: ValueTerm]
      : Factory3[Seq[Int],
                 ArrayTree[LocalElement],
                 Factory1[TreeApi {
                            type TermIn[C <: Category] = LocalElement#TermIn[C]
                          },
                          LocalElement],
                 ArrayTerm {
                   type Element = LocalElement
                 }]

    def parameter[ElementType <: ValueType](id: Any, elementType: ElementType, shape: Int*): ArrayTerm {
      type Element = elementType.ThisTerm
    } = {
      val parameterTree = ArrayParameter[elementType.type](id, elementType, shape: _*)
      array
        .factory[elementType.ThisTerm]
        .newInstance(
          shape,
          parameterTree.asInstanceOf[ArrayTree[elementType.ThisTerm]],
          elementType.factory
        )
    }

  }

  type ArrayCompanion <: ArrayCompanionApi
}
