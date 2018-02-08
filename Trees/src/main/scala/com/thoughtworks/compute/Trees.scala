package com.thoughtworks.compute

import java.util.IdentityHashMap

import com.github.ghik.silencer.silent
import com.thoughtworks.compute.Expressions._
import com.thoughtworks.compute.NDimensionalAffineTransform.MatrixData
import com.thoughtworks.feature.Factory.{Factory1, Factory2, inject}

import scala.annotation.meta.companionObject
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.util.hashing.MurmurHash3

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Expressions {

  final class StructuralComparisonContext extends IdentityHashMap[Tree, Tree]

  private def childHashCode(child: Any, context: HashCodeContext): Int = {
    child match {
      case childTree: Tree @unchecked =>
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

  trait Operator extends Tree { thisOperator =>

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

    private def isSameChild(left: Any, right: Any, map: StructuralComparisonContext): Boolean = {
      left match {
        case left: Tree @unchecked =>
          right match {
            case right: Tree @unchecked =>
              left.isSameStructure(right, map)
            case _ =>
              false
          }
        case left: Array[_] =>
          right match {
            case right: Array[_] =>
              val leftLength = left.length
              val rightLength = right.length
              @tailrec def arrayLoop(start: Int): Boolean = {
                if (start < leftLength) {
                  if (isSameChild(left(start), right(start), map)) {
                    arrayLoop(start + 1)
                  } else {
                    false
                  }
                } else {
                  true
                }
              }
              leftLength == rightLength && arrayLoop(0)
            case _ =>
              false
          }
        case _ =>
          left == right
      }
    }
    def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean = {

      map.get(this) match {
        case null =>
          this.getClass == that.getClass && {
            assert(this.productArity == that.productArity)
            map.put(this, that)
            val productArity: Int = this.productArity
            @tailrec
            def fieldLoop(from: Int): Boolean = {
              if (from < productArity) {
                if (isSameChild(this.productElement(from), that.productElement(from), map)) {
                  fieldLoop(from + 1)
                } else {
                  false
                }
              } else {
                true
              }
            }
            fieldLoop(0)
          }
        case existing =>
          existing eq that
      }
    }
  }

  trait Parameter extends Tree { thisParameter =>

    val id: Any

  }

  final class HashCodeContext extends IdentityHashMap[Tree, Int] {
    var numberOfParameters = 0
  }

  final class AlphaConversionContext extends IdentityHashMap[Tree, Tree]

  trait Tree extends Product { thisTree =>
    type TermIn[C <: Category]

    def export(foreignCategory: Category, context: ExportContext): TermIn[foreignCategory.type]

    // TODO: alphaConversion

    def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean

    def structuralHashCode(context: HashCodeContext): Int

    def alphaConversion(context: AlphaConversionContext): Tree

  }

  final class ExportContext extends IdentityHashMap[Tree, Any]

  protected trait ExpressionApi extends super.ExpressionApi { thisExpression =>
  }

  protected trait TermApi extends ExpressionApi with super.TermApi { thisTerm: Term =>

    def alphaConversion: ThisTerm

    def in(foreignCategory: Category): TermIn[foreignCategory.type] = {
      tree.export(foreignCategory, new ExportContext)
    }

    val tree: Tree {
      type TermIn[C <: Category] = thisTerm.TermIn[C]
    }

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

      def in(foreignCategory: Category): TypeIn[foreignCategory.type]

      def factory: Factory1[Tree { type TermIn[C <: Category] = ThisTerm#TermIn[C] }, ThisTerm]

    }

    type ValueType <: (Type with Any) with ValueTypeApi

    protected trait ValueTermApi extends TermApi with super.ValueTermApi { thisValue: ValueTerm =>

      def factory: Factory1[Tree { type TermIn[C <: Category] = thisValue.TermIn[C] }, ThisTerm]

      def alphaConversion: ThisTerm = {
        factory.newInstance(
          tree
            .alphaConversion(new AlphaConversionContext)
            .asInstanceOf[Tree { type TermIn[C <: Category] = thisValue.TermIn[C] }])
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

      def factory: Factory1[Tree { type TermIn[C <: Category] = C#FloatTerm }, ThisTerm] = {
        float.factory
      }

      def unary_- : FloatTerm = factory.newInstance(UnaryMinus(tree))

      def unary_+ : FloatTerm = factory.newInstance(UnaryPlus(tree))

      def +(rightHandSide: FloatTerm): FloatTerm = factory.newInstance(Plus(tree, rightHandSide.tree))
      def -(rightHandSide: FloatTerm): FloatTerm = factory.newInstance(Minus(tree, rightHandSide.tree))
      def *(rightHandSide: FloatTerm): FloatTerm = factory.newInstance(Times(tree, rightHandSide.tree))
      def /(rightHandSide: FloatTerm): FloatTerm = factory.newInstance(Div(tree, rightHandSide.tree))
      def %(rightHandSide: FloatTerm): FloatTerm = factory.newInstance(Percent(tree, rightHandSide.tree))
    }

    type FloatTerm <: (ValueTerm with Any) with FloatTermApi

    type FloatTree = Tree {
      type TermIn[C <: Category] = C#FloatTerm
    }

    protected trait FloatOperatorApi extends Tree with Operator {
      type TermIn[C <: Category] = C#FloatTerm

    }

    @(silent @companionObject)
    final case class FloatParameter(id: Any) extends Tree with Parameter { thisParameter =>
      type TermIn[C <: Category] = C#FloatTerm
      def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean = {
        map.get(this) match {
          case null =>
            map.put(this, that)
            true
          case existing =>
            existing eq that
        }
      }

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

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = {
          val newId = new AnyRef {
            override val toString: String = raw"""α-converted(${thisParameter.toString})"""
          }
          FloatParameter(newId)
        }
        context.asScala.getOrElseUpdate(this, converted)
      }

    }

    @(silent @companionObject)
    final case class FloatLiteral(value: Float) extends FloatOperatorApi {

      def alphaConversion(context: AlphaConversionContext): Tree = this

      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, foreignCategory.float.literal(value))
          .asInstanceOf[foreignCategory.FloatTerm]
      }
    }

    @(silent @companionObject)
    final case class Plus(operand0: FloatTree, operand1: FloatTree) extends FloatOperatorApi {

      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) + operand1.export(foreignCategory, context))
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(operand0.alphaConversion(context).asInstanceOf[FloatTree],
               operand1.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class Minus(operand0: FloatTree, operand1: FloatTree) extends FloatOperatorApi {
      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) - operand1.export(foreignCategory, context))
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(operand0.alphaConversion(context).asInstanceOf[FloatTree],
               operand1.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class Times(operand0: FloatTree, operand1: FloatTree) extends FloatOperatorApi {
      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) * operand1.export(foreignCategory, context))
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(operand0.alphaConversion(context).asInstanceOf[FloatTree],
               operand1.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class Div(operand0: FloatTree, operand1: FloatTree) extends FloatOperatorApi {
      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) / operand1.export(foreignCategory, context))
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(operand0.alphaConversion(context).asInstanceOf[FloatTree],
               operand1.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class Percent(operand0: FloatTree, operand1: FloatTree) extends FloatOperatorApi {
      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) % operand1.export(foreignCategory, context))
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(operand0.alphaConversion(context).asInstanceOf[FloatTree],
               operand1.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class UnaryMinus(operand: FloatTree) extends FloatOperatorApi {
      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand.export(foreignCategory, context).unary_-)
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class UnaryPlus(operand: FloatTree) extends FloatOperatorApi {
      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, operand.export(foreignCategory, context).unary_+)
          .asInstanceOf[foreignCategory.FloatTerm]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand.alphaConversion(context).asInstanceOf[FloatTree])
        context.asScala.getOrElseUpdate(this, converted)
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
      def factory: Factory1[Tree { type TermIn[C <: Category] = ThisTerm#TermIn[C] }, ThisTerm]

    }

    type FloatType <: (ValueType with Any) with FloatTypeApi

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait ArrayTrees extends Arrays with ValueTrees {

    type ArrayTree[LocalElement <: ValueTerm] = Tree {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }
    }

    @(silent @companionObject)
    final case class Extract[LocalElement <: ValueTerm](array: ArrayTree[LocalElement]) extends Tree with Operator {
      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, array.export(foreignCategory, map).extract)
          .asInstanceOf[TermIn[foreignCategory.type]]
      }
      type TermIn[C <: Category] = LocalElement#TermIn[C]

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(array.alphaConversion(context).asInstanceOf[ArrayTree[LocalElement]])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class Transform[LocalElement <: ValueTerm](array: ArrayTree[LocalElement], matrix: MatrixData)
        extends Tree
        with Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, array.export(foreignCategory, map).transform(matrix))
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(array.alphaConversion(context).asInstanceOf[ArrayTree[LocalElement]], matrix)
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    protected trait ArrayTermApi extends super.ArrayTermApi with TermApi { thisArray: ArrayTerm =>

      def alphaConversion: ThisTerm = {
        array
          .factory[Element]
          .newInstance(tree
                         .alphaConversion(new AlphaConversionContext)
                         .asInstanceOf[Tree { type TermIn[C <: Category] = thisArray.TermIn[C] }],
                       valueFactory)
          .asInstanceOf[ThisTerm]
      }

      val valueFactory: Factory1[Tree {
                                   type TermIn[C <: Category] = thisArray.Element#TermIn[C]
                                 },
                                 Element]

      def extract: Element = {
        valueFactory.newInstance(Extract(tree))
      }

      def transform(matrix: MatrixData): ThisTerm = {
        val translatedTree = Transform[Element](tree, matrix)
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

    @(silent @companionObject)
    final case class Fill[LocalElement <: ValueTerm](element: Tree {
      type TermIn[C <: Category] = LocalElement#TermIn[C]
    }) extends Tree
        with Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, element.export(foreignCategory, map).fill)
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = {
          Fill[LocalElement](
            element
              .alphaConversion(context)
              .asInstanceOf[Tree {
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
          tree.asInstanceOf[Tree { type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C] }])
        array
          .factory[ThisTerm]
          .newInstance(
            fillTree,
            thisValue.factory
              .asInstanceOf[Factory1[Tree {
                                       type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C]
                                     },
                                     thisValue.ThisTerm]]
          )
      }

    }

    type ValueTerm <: (Term with Any) with ValueTermApi

    @(silent @companionObject)
    final case class ArrayParameter[LocalElement <: ValueTerm](id: Any, padding: Tree {
      type TermIn[C <: Category] = LocalElement#TermIn[C]
    }, shape: Array[Int])
        extends Tree
        with Parameter {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean = {
        map.get(this) match {
          case null =>
            that match {
              case ArrayParameter(thatId, thatPadding, thatShape)
                  if padding == thatPadding && java.util.Arrays.equals(shape, thatShape) =>
                map.put(this, that)
                true
              case _ =>
                false
            }
          case existing =>
            existing eq that
        }
      }

      def structuralHashCode(context: HashCodeContext): Int = {
        context.asScala.getOrElseUpdate(
          this, {
            val h = context.numberOfParameters
            context.numberOfParameters = h + 1

            MurmurHash3.finalizeHash(
              MurmurHash3.mixLast(
                MurmurHash3.mix(
                  h,
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
              .parameter[padding.TermIn[foreignCategory.type]](id, padding.export(foreignCategory, map), shape))
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = {
          val convertedPadding = padding
            .alphaConversion(context)
            .asInstanceOf[Tree {
              type TermIn[C <: Category] = LocalElement#TermIn[C]
            }]
          ArrayParameter[LocalElement](id, convertedPadding, shape)
        }
        context.asScala.getOrElseUpdate(this, converted)

      }
    }

    protected trait ArrayCompanionApi extends super.ArrayCompanionApi {

      @inject def factory[LocalElement <: ValueTerm]
        : Factory2[ArrayTree[LocalElement],
                   Factory1[Tree {
                              type TermIn[C <: Category] = LocalElement#TermIn[C]
                            },
                            LocalElement],
                   ArrayTerm {
                     type Element = LocalElement
                   }]

      def parameter[Element0 <: ValueTerm](id: Any, padding: Element0, shape: Array[Int]): ArrayTerm {
        type Element = Element0
      } = {
        val parameterTree = ArrayParameter[Element0](
          id,
          padding.tree.asInstanceOf[Tree { type TermIn[C <: Category] = Element0#TermIn[C] }],
          shape)
        array
          .factory[Element0]
          .newInstance(
            parameterTree.asInstanceOf[ArrayTree[Element0]],
            padding.factory.asInstanceOf[Factory1[Tree {
                                                    type TermIn[C <: Category] = Element0#TermIn[C]
                                                  },
                                                  Element0]]
          )
      }

    }

    type ArrayCompanion <: ArrayCompanionApi
  }

  /**
    * @author 杨博 (Yang Bo)
    */
  @deprecated(message = "Use [[AllTrees]] instead", since = "0.2.0")
  trait FloatArrayTrees extends ArrayTrees with FloatTrees with FloatArrays

  trait TupleTrees extends ValueTrees with Tuples {
    // TODO: Rename XxxApi in this file to XxxTreeType or XxxTreeTerm
    protected trait TupleTreeType extends super.TupleTypeApi with ValueTypeApi {
      override def hashCode(): Int = ???

      override def equals(that: scala.Any): Boolean = ???
    }

    type TupleType <: (ValueType with Any) with TupleTreeType

    protected trait TupleTermApi extends super[ValueTrees].ValueTermApi with super[Tuples].TupleTermApi {
      this: TupleTerm =>
      def split: Seq[Element] = {
        ???
      }
    }
    type TupleTerm <: (ValueTerm with Any) with TupleTermApi

    protected trait TupleSingletonApi extends super.TupleSingletonApi {

      def parameter[ElementType <: ValueType](id: Any, elementType: ElementType, numberOfElements: Int): TupleTerm {
        type Element = elementType.ThisTerm
      } = {
        ???
      }

      def concatenate[Element0 <: ValueTerm](elements: Element0*): TupleTerm { type Element = Element0 } = {
        ???
      }

    }

    type TupleSingleton <: TupleSingletonApi

  }

  trait AllTrees extends TupleTrees with FloatTrees with ArrayTrees with AllExpressions

}
