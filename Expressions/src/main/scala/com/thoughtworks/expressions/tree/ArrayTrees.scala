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
    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, array.export(foreignCategory, map).extract)
        .asInstanceOf[TermIn[foreignCategory.type]]
    }
    type TermIn[C <: Category] = LocalElement#TermIn[C]

    def alphaConversion(context: AlphaConversionContext): TreeApi = {
      def converted = Extract(array.alphaConversion(context).asInstanceOf[ArrayTree[LocalElement]])
      context.asScala.getOrElseUpdate(this, converted)
    }
  }

  final case class Translate[LocalElement <: ValueTerm](array: ArrayTree[LocalElement], offset: Int*)
      extends TreeApi
      with Operator {
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = LocalElement#TermIn[C]
    }

    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, array.export(foreignCategory, map).translate(offset: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]
    }

    def alphaConversion(context: AlphaConversionContext): TreeApi = {
      def converted = Translate(array.alphaConversion(context).asInstanceOf[ArrayTree[LocalElement]])
      context.asScala.getOrElseUpdate(this, converted)
    }
  }

  protected trait ArrayTermApi extends super.ArrayTermApi with TermApi { thisArray: ArrayTerm =>

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
        .parameterFactory[Element]
        .newInstance(
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

    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, element.export(foreignCategory, map).fill)
        .asInstanceOf[TermIn[foreignCategory.type]]
    }

    def alphaConversion(context: AlphaConversionContext): TreeApi = {
      def converted = {
        Fill[LocalElement](element
                             .alphaConversion(context)
                             .asInstanceOf[TreeApi {
                               type TermIn[C <: Category] = LocalElement#TermIn[C]
                             }],
                           shape: _*)
      }
      context.asScala.getOrElseUpdate(this, converted)

    }
  }

  protected trait ValueTermApi extends super[Arrays].ValueTermApi with super[ValueTrees].ValueTermApi with TermApi {
    thisValue: ValueTerm =>

    def fill: ArrayTerm {
      type Element = thisValue.ThisTerm
    } = {
      val fillTree = Fill[thisValue.ThisTerm](
        tree.asInstanceOf[TreeApi { type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C] }])
      array
        .parameterFactory[ThisTerm]
        .newInstance(
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
      with Parameter { thisParameter =>
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = elementType.TermIn[C]
    }

    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
      map.asScala
        .getOrElseUpdate(this, foreignCategory.array.parameter(id, elementType.in(foreignCategory), shape: _*))
        .asInstanceOf[TermIn[foreignCategory.type]]

    }

    def alphaConversion(context: AlphaConversionContext): TreeApi = {
      def converted = {
        val newId = new AnyRef {
          override val toString: String = raw"""α-converted(${thisParameter.toString})"""
        }
        ArrayParameter(newId, elementType, shape: _*)
      }
      context.asScala.getOrElseUpdate(this, converted)
    }

  }

  protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

    @inject def parameterFactory[LocalElement <: ValueTerm]
      : Factory2[ArrayTree[LocalElement],
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
        .parameterFactory[elementType.ThisTerm]
        .newInstance(
          parameterTree.asInstanceOf[ArrayTree[elementType.ThisTerm]],
          elementType.factory
        )
    }

  }

  type ArrayCompanion <: ArrayCompanionApi
}
