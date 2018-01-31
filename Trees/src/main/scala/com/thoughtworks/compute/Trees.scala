package com.thoughtworks.compute

import java.util.IdentityHashMap

import com.thoughtworks.compute.Expressions.{Arrays, FloatArrays, Floats, Values}
import com.thoughtworks.feature.Factory.{Factory1, Factory2, inject}
import org.apache.commons.math3.linear.RealMatrix

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.util.hashing.MurmurHash3

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Expressions {

  final class StructuralComparisonContext extends IdentityHashMap[TreeApi, TreeApi]

  private def childHashCode(child: Any, context: HashCodeContext): Int = {
    child match {
      case childTree: TreeApi =>
        childTree.structuralHashCode(context)
      case childArray: Array[_] =>
        arrayHashCode(childArray, context)
      case _ =>
        child.##
    }
  }

  private def arrayHashCode[@specialized A](childArray: Array[A], context: HashCodeContext): Int = {
    val length = childArray.length
    if (length == 0) {
      MurmurHash3.arraySeed
    } else {
      val last = length - 1
      @tailrec
      def arrayLoop(h: Int, i: Int): Int = {
        if (i < last) {
          arrayLoop(h = MurmurHash3.mix(h, childHashCode(childArray(i), context)), i = i + 1)
        } else {
          MurmurHash3.finalizeHash(MurmurHash3.mixLast(h, childHashCode(childArray(i), context)), length)
        }
      }
      arrayLoop(MurmurHash3.arraySeed, 0)
    }
  }

  trait Operator extends TreeApi { thisOperator =>

    def structuralHashCode(context: HashCodeContext): Int = {
      val productArity: Int = this.productArity
      if (productArity == 0) {
        productPrefix.##
      } else {
        context.asScala.getOrElseUpdate(
          this, {
            val last = productArity - 1
            @tailrec
            def loop(h: Int = productPrefix.hashCode, i: Int = 0): Int = {
              if (i < last) {
                loop(h = MurmurHash3.mix(h, childHashCode(productElement(i), context)), i = i + 1)
              } else {
                MurmurHash3.finalizeHash(MurmurHash3.mixLast(h, childHashCode(productElement(i), context)),
                                         productArity)
              }
            }
            loop()
          }
        )
      }
    }

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean = {
      map.get(this) match {
        case null =>
          this.getClass == that.getClass && {
            assert(this.productArity == that.productArity)
            map.put(this, that)
            val productArity: Int = this.productArity
            @tailrec
            def sameFields(start: Int = 0): Boolean = {
              if (start < productArity) {
                productElement(start) match {
                  case left: TreeApi =>
                    that.productElement(start) match {
                      case right: TreeApi =>
                        if (left.isSameStructure(right, map)) {
                          sameFields(start = start + 1)
                        } else {
                          false
                        }
                      case _ =>
                        false
                    }
                  case _ =>
                    false
                }
              } else {
                true
              }
            }
            sameFields()
          }
        case existing =>
          existing eq that
      }
    }
  }

  trait Parameter extends TreeApi { thisParameter =>

    val id: Any

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean = {
      map.get(this) match {
        case null =>
          map.put(this, that)
          true
        case existing =>
          existing eq that
      }
    }
  }

  final class HashCodeContext extends IdentityHashMap[TreeApi, Int] {
    var numberOfParameters = 0
  }

  final class AlphaConversionContext extends IdentityHashMap[TreeApi, TreeApi]

  trait TreeApi extends Product { thisTree =>
    type TermIn[C <: Category]

    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type]

    // TODO: alphaConversion

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean

    def structuralHashCode(context: HashCodeContext): Int

    def alphaConversion(context: AlphaConversionContext): TreeApi

  }

  final class ExportContext extends IdentityHashMap[TreeApi, Any]

  protected trait ExpressionApi extends super.ExpressionApi { thisExpression =>
    type Tree = TreeApi {
      type TermIn[C <: Category] = thisExpression.TermIn[C]
    }
  }

  protected trait TermApi extends ExpressionApi with super.TermApi { thisTree: Term =>

    def alphaConversion: ThisTerm

    def in(foreignCategory: Category): TermIn[foreignCategory.type] = {
      tree.export(foreignCategory, new ExportContext)
    }

    val tree: Tree

  }

  type Term <: TermApi

  protected trait TypeApi extends ExpressionApi with super.TypeApi
  type Type <: TypeApi

}

object Trees {

  /**
    * @author 杨博 (Yang Bo)
    */
  trait ValueTrees extends Values with Trees {

    protected trait ValueTypeApi extends super.ValueTypeApi {

      override def equals(that: scala.Any): Boolean = {
        that != null && that.getClass == this.getClass
      }

      def in(foreignCategory: Category): TypeIn[foreignCategory.type]

      def factory: Factory1[TreeApi { type TermIn[C <: Category] = ThisTerm#TermIn[C] }, ThisTerm]

    }

    type ValueType <: (Type with Any) with ValueTypeApi

    protected trait ValueTermApi extends TermApi with super.ValueTermApi { thisValue: ValueTerm =>

      def factory: Factory1[TreeApi { type TermIn[C <: Category] = thisValue.TermIn[C] }, ThisTerm]

      def alphaConversion: ThisTerm = {
        factory.newInstance(tree.alphaConversion(new AlphaConversionContext).asInstanceOf[Tree])
      }
    }

    type ValueTerm <: (Term with Any) with ValueTermApi

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait StructuralTrees extends Trees {

    protected trait TermApi extends super.TermApi { this: Term =>
      override def hashCode(): Int = {
        tree.structuralHashCode(new HashCodeContext)
      }

      override def equals(that: Any): Boolean = {
        that match {
          case that: TermApi =>
            tree.isSameStructure(that.tree, new StructuralComparisonContext)
          case _ =>
            false
        }
      }
    }

    type Term <: TermApi
  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait FloatTrees extends Floats with ValueTrees {

    protected trait FloatTermApi extends super.FloatTermApi with ValueTermApi with FloatExpressionApi {
      thisFloat: FloatTerm =>

      def factory: Factory1[TreeApi { type TermIn[C <: Category] = C#FloatTerm }, ThisTerm] = {
        float.factory
      }
    }

    type FloatTerm <: (ValueTerm with Any) with FloatTermApi

    final case class FloatParameter(id: Any) extends TreeApi with Parameter { thisParameter =>
      type TermIn[C <: Category] = C#FloatTerm

      def structuralHashCode(context: HashCodeContext): Int = {
        context.asScala.getOrElseUpdate(this, {
          val newId = context.numberOfParameters
          context.numberOfParameters = newId + 1
          newId
        })
      }

      def export(foreignCategory: Category, map: ExportContext): foreignCategory.FloatTerm = {
        map.asScala
          .getOrElseUpdate(this, foreignCategory.float.parameter(id))
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): TreeApi = {
        def converted = {
          val newId = new AnyRef {
            override val toString: String = raw"""α-converted(${thisParameter.toString})"""
          }
          FloatParameter(newId)
        }
        context.asScala.getOrElseUpdate(this, converted)
      }

    }

    final case class FloatLiteral(value: Float) extends TreeApi with Operator {

      def alphaConversion(context: AlphaConversionContext): TreeApi = this

      type TermIn[C <: Category] = C#FloatTerm

      def export(foreignCategory: Category, map: ExportContext): foreignCategory.FloatTerm = {
        map.asScala
          .getOrElseUpdate(this, foreignCategory.float.literal(value))
          .asInstanceOf[foreignCategory.FloatTerm]
      }
    }

    protected trait FloatTypeApi extends ValueTypeApi with super.FloatTypeApi with FloatExpressionApi {
      def in(foreignCategory: Category): TypeIn[foreignCategory.type] = {
        foreignCategory.float
      }

      def literal(value: Float): FloatTerm = {
        factory.newInstance(FloatLiteral(value))
      }

      def parameter(id: Any): FloatTerm = {
        factory.newInstance(FloatParameter(id))
      }

      @inject
      def factory: Factory1[TreeApi { type TermIn[C <: Category] = ThisTerm#TermIn[C] }, ThisTerm]

      override def hashCode(): Int = {
        "float".##
      }
    }

    type FloatType <: (ValueType with Any) with FloatTypeApi

  }

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

    final case class Transform[LocalElement <: ValueTerm](array: ArrayTree[LocalElement], matrix: RealMatrix)
        extends TreeApi
        with Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, array.export(foreignCategory, map).transform(matrix))
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): TreeApi = {
        def converted = Transform(array.alphaConversion(context).asInstanceOf[ArrayTree[LocalElement]], matrix)
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    final case class Translate[LocalElement <: ValueTerm](array: ArrayTree[LocalElement], offset: Array[Int])
        extends TreeApi
        with Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, array.export(foreignCategory, map).translate(offset))
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): TreeApi = {
        def converted = Translate(array.alphaConversion(context).asInstanceOf[ArrayTree[LocalElement]], offset)
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    protected trait ArrayTermApi extends super.ArrayTermApi with TermApi { thisArray: ArrayTerm =>

      def alphaConversion: ThisTerm = {
        array
          .factory[Element]
          .newInstance(tree.alphaConversion(new AlphaConversionContext).asInstanceOf[Tree], valueFactory)
          .asInstanceOf[ThisTerm]
      }

      val valueFactory: Factory1[TreeApi {
                                   type TermIn[C <: Category] = thisArray.Element#TermIn[C]
                                 },
                                 Element]

      def extract: Element = {
        valueFactory.newInstance(Extract(tree))
      }

      def transform(matrix: RealMatrix): ThisTerm = {
        val translatedTree = Transform[Element](tree, matrix)
        array
          .factory[Element]
          .newInstance(
            translatedTree,
            valueFactory
          )
          .asInstanceOf[ThisTerm]
      }

      def translate(offset: Array[Int]): ThisTerm = {
        val translatedTree = Translate[Element](tree, offset)
        array
          .factory[Element]
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
    }) extends TreeApi
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
          Fill[LocalElement](
            element
              .alphaConversion(context)
              .asInstanceOf[TreeApi {
                type TermIn[C <: Category] = LocalElement#TermIn[C]
              }])
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
          .factory[ThisTerm]
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

    final case class ArrayParameter[Padding, ElementType <: ValueType { type JvmValue = Padding }](
        id: Any,
        elementType: ElementType,
        padding: Padding,
        shape: Array[Int])
        extends TreeApi
        with Parameter { thisParameter =>

      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = elementType.TermIn[C]
      }

      def structuralHashCode(context: HashCodeContext): Int = {
        context.asScala.getOrElseUpdate(
          this, {
            val h = context.numberOfParameters
            context.numberOfParameters = h + 1

            MurmurHash3.finalizeHash(
              MurmurHash3.mixLast(
                MurmurHash3.mix(
                  MurmurHash3.mix(h, elementType.##),
                  padding.##
                ),
                MurmurHash3.arrayHash(shape)
              ),
              3
            )
          }
        )

      }

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(
            this,
            foreignCategory.array
              .parameter[Padding, elementType.TypeIn[foreignCategory.type]](id,
                                                                            elementType.in(foreignCategory),
                                                                            padding,
                                                                            shape))
          .asInstanceOf[TermIn[foreignCategory.type]]

      }

      def alphaConversion(context: AlphaConversionContext): TreeApi = {
        def converted = {
          val newId = new AnyRef {
            override val toString: String = raw"""α-converted(${thisParameter.toString})"""
          }
          ArrayParameter(newId, elementType, padding, shape)
        }
        context.asScala.getOrElseUpdate(this, converted)
      }

    }

    protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

      @inject def factory[LocalElement <: ValueTerm]
        : Factory2[ArrayTree[LocalElement],
                   Factory1[TreeApi {
                              type TermIn[C <: Category] = LocalElement#TermIn[C]
                            },
                            LocalElement],
                   ArrayTerm {
                     type Element = LocalElement
                   }]

      def parameter[Padding, ElementType <: ValueType { type JvmValue = Padding }](id: Any,
                                                                                   elementType: ElementType,
                                                                                   padding: Padding,
                                                                                   shape: Array[Int]): ArrayTerm {
        type Element = elementType.ThisTerm
      } = {
        val parameterTree = ArrayParameter[Padding, elementType.type](id, elementType, padding, shape)
        array
          .factory[elementType.ThisTerm]
          .newInstance(
            parameterTree.asInstanceOf[ArrayTree[elementType.ThisTerm]],
            elementType.factory
          )
      }

    }

    type ArrayCompanion <: ArrayCompanionApi
  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait FloatArrayTrees extends ArrayTrees with FloatTrees with FloatArrays

}
