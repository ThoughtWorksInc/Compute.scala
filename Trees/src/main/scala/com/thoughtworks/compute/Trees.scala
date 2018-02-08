package com.thoughtworks.compute

import java.util.IdentityHashMap

import com.github.ghik.silencer.silent
import com.thoughtworks.compute.Expressions._
import com.thoughtworks.compute.NDimensionalAffineTransform.MatrixData
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

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

    def cast[TermIn0[C <: Category]]: Tree { type TermIn[C <: Category] = TermIn0[C] } = {
      this.asInstanceOf[Tree { type TermIn[C <: Category] = TermIn0[C] }]
    }

    def export(foreignCategory: Category, context: ExportContext): TermIn[foreignCategory.type]

    // TODO: alphaConversion

    def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean

    def structuralHashCode(context: HashCodeContext): Int

    def alphaConversion(context: AlphaConversionContext): Tree

  }

  final class ExportContext extends IdentityHashMap[Tree, Any]

  protected trait TreeTerm extends ExpressionApi with TermApi { thisTerm: Term =>

    def alphaConversion: ThisTerm

    @inline
    final def in(foreignCategory: Category): TermIn[foreignCategory.type] = {
      tree.export(foreignCategory, new ExportContext)
    }

    @inline
    final def tree: Tree { type TermIn[C <: Category] = thisTerm.TermIn[C] } =
      tree0.asInstanceOf[Tree {
        type TermIn[C <: Category] = thisTerm.TermIn[C]
      }]

    protected val tree0: Tree

  }

  type Term <: TreeTerm

}

object Trees {

  /**
    * @author 杨博 (Yang Bo)
    */
  trait ValueTrees extends Values with Trees {

    protected trait ValueTreeType extends ValueTypeApi { thisValueType =>

      def in(foreignCategory: Category): TypeIn[foreignCategory.type]

      def term(tree: Tree): ThisTerm

    }

    type ValueType <: (Type with Any) with ValueTreeType

    protected trait ValueTreeTerm extends TermApi with ValueTermApi { thisValue: ValueTerm =>

      def valueType: ThisType

      def alphaConversion: ThisTerm = {
        valueType.term(tree.alphaConversion(new AlphaConversionContext)).asInstanceOf[ThisTerm]
      }
    }

    type ValueTerm <: (Term with Any) with ValueTreeTerm

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait StructuralTrees extends Trees {

    protected trait StructuralTreeTerm extends TreeTerm { this: Term =>
      override def hashCode(): Int = {
        tree.structuralHashCode(new HashCodeContext)
      }

      override def equals(that: Any): Boolean = {
        that match {
          case that: StructuralTreeTerm =>
            tree.isSameStructure(that.tree, new StructuralComparisonContext)
          case _ =>
            false
        }
      }
    }

    type Term <: StructuralTreeTerm
  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait FloatTrees extends Floats with ValueTrees {

    protected trait FloatTreeTerm extends FloatTermApi with ValueTreeTerm with FloatExpressionApi {
      thisFloat: FloatTerm =>

      def valueType: ThisType = float

      def unary_- : FloatTerm = float.term(UnaryMinus(tree))

      def unary_+ : FloatTerm = float.term(UnaryPlus(tree))

      def +(rightHandSide: FloatTerm): FloatTerm = float.term(Plus(tree, rightHandSide.tree))
      def -(rightHandSide: FloatTerm): FloatTerm = float.term(Minus(tree, rightHandSide.tree))
      def *(rightHandSide: FloatTerm): FloatTerm = float.term(Times(tree, rightHandSide.tree))
      def /(rightHandSide: FloatTerm): FloatTerm = float.term(Div(tree, rightHandSide.tree))
      def %(rightHandSide: FloatTerm): FloatTerm = float.term(Percent(tree, rightHandSide.tree))
    }

    type FloatTerm <: (ValueTerm with Any) with FloatTreeTerm

    type FloatTree = Tree {
      type TermIn[C <: Category] = C#FloatTerm
    }

    protected trait FloatOperator extends Operator {
      type TermIn[C <: Category] = C#FloatTerm

    }

    @(silent @companionObject)
    final case class FloatParameter(id: Any) extends Parameter { thisParameter =>
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
            override val toString: String = raw"""α-converted(${id.toString})"""
          }
          FloatParameter(newId)
        }
        context.asScala.getOrElseUpdate(this, converted)
      }

    }

    @(silent @companionObject)
    final case class FloatLiteral(value: Float) extends FloatOperator {

      def alphaConversion(context: AlphaConversionContext): Tree = this

      def export(foreignCategory: Category, context: ExportContext): foreignCategory.FloatTerm = {
        context.asScala
          .getOrElseUpdate(this, foreignCategory.float.literal(value))
          .asInstanceOf[foreignCategory.FloatTerm]
      }
    }

    @(silent @companionObject)
    final case class Plus(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {

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
    final case class Minus(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
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
    final case class Times(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
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
    final case class Div(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
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
    final case class Percent(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
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
    final case class UnaryMinus(operand: FloatTree) extends FloatOperator {
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
    final case class UnaryPlus(operand: FloatTree) extends FloatOperator {
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

    protected trait FloatTreeType extends ValueTreeType with FloatTypeApi {
      @inline
      def in(foreignCategory: Category): TypeIn[foreignCategory.type] = {
        foreignCategory.float
      }

      @inject
      protected def termFactory: Factory1[Tree, ThisTerm]

      @inline
      def literal(value: JvmValue): ThisTerm = {
        termFactory.newInstance(FloatLiteral(value))
      }

      @inline
      def parameter(id: Any): ThisTerm = {
        termFactory.newInstance(FloatParameter(id))
      }
      @inline
      def term(tree: Tree): ThisTerm = termFactory.newInstance(tree)
    }

    type FloatType <: (ValueType with Any) with FloatTreeType

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait ArrayTrees extends Arrays with ValueTrees {

    @(silent @companionObject)
    final case class Extract[LocalElement <: ValueTerm](array: Tree {
      type TermIn[C <: Category] = C#ArrayTerm { type Element = LocalElement#TermIn[C] }
    }) extends Operator {

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, array.export(foreignCategory, map).extract)
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      type TermIn[C <: Category] = LocalElement#TermIn[C]

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(
            array
              .alphaConversion(context)
              .asInstanceOf[Tree {
                type TermIn[C <: Category] = C#ArrayTerm { type Element = LocalElement#TermIn[C] }
              }])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @(silent @companionObject)
    final case class Transform[LocalElement <: ValueTerm](array: Tree {
      type TermIn[C <: Category] = C#ArrayTerm { type Element = LocalElement#TermIn[C] }
    }, matrix: MatrixData)
        extends Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type] = {
        map.asScala
          .getOrElseUpdate(this, array.export(foreignCategory, map).transform(matrix))
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted =
          copy(
            array
              .alphaConversion(context)
              .asInstanceOf[Tree {
                type TermIn[C <: Category] = C#ArrayTerm { type Element = LocalElement#TermIn[C] }
              }])
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    protected trait ArrayTreeTerm extends ArrayTermApi with TreeTerm { thisArray: ArrayTerm =>

      def alphaConversion: ThisTerm = {
        arrayTermFactory[Element]
          .newInstance(elementType, tree.alphaConversion(new AlphaConversionContext))
          .asInstanceOf[ThisTerm]
      }

      val elementType: ValueType

      def extract: Element = {
        elementType.term(Extract[Element](tree)).asInstanceOf[Element]
      }

      def transform(matrix: MatrixData): ThisTerm = {
        val translatedTree = Transform[Element](tree, matrix)
        arrayTermFactory[Element]
          .newInstance(elementType, translatedTree)
          .asInstanceOf[ThisTerm]
      }

    }

    type ArrayTerm <: (Term with Any) with ArrayTreeTerm

    @(silent @companionObject)
    final case class Fill[LocalElement <: ValueTerm](element: Tree {
      type TermIn[C <: Category] = LocalElement#TermIn[C]
    }) extends Operator {
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

    protected trait ElementTreeTerm extends ElementTermApi with ValueTreeTerm {
      thisValue: ValueTerm =>

      def fill: ArrayTerm {
        type Element = thisValue.ThisTerm
      } = {
        val fillTree = Fill[thisValue.ThisTerm](
          tree.asInstanceOf[Tree { type TermIn[C <: Category] = thisValue.ThisTerm#TermIn[C] }])
        arrayTermFactory[ThisTerm]
          .newInstance(thisValue.valueType, fillTree)
      }

    }

    type ValueTerm <: (Term with Any) with ElementTreeTerm

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
          val newId = new AnyRef {
            override val toString: String = raw"""α-converted(${id.toString})"""
          }
          ArrayParameter[LocalElement](newId, convertedPadding, shape)
        }
        context.asScala.getOrElseUpdate(this, converted)

      }
    }

    @inject
    protected def arrayTermFactory[LocalElement <: ValueTerm]: Factory2[ValueType,
                                                                        Tree,
                                                                        ArrayTerm {
                                                                          type Element = LocalElement
                                                                        }]

    protected trait TreeArrayCompanion extends ArrayCompanionApi {

      def parameter[Element0 <: ValueTerm](id: Any, padding: Element0, shape: Array[Int]): ArrayTerm {
        type Element = Element0
      } = {
        val parameterTree = ArrayParameter[Element0](
          id,
          padding.tree.asInstanceOf[Tree { type TermIn[C <: Category] = Element0#TermIn[C] }],
          shape
        )
        arrayTermFactory[Element0].newInstance(padding.valueType, parameterTree)
      }

    }

    type ArrayCompanion <: TreeArrayCompanion
  }

  /**
    * @author 杨博 (Yang Bo)
    */
  @deprecated(message = "Use [[AllTrees]] instead", since = "0.2.0")
  trait FloatArrayTrees extends ArrayTrees with FloatTrees with FloatArrays

  trait TupleTrees extends ValueTrees with Tuples {
    // TODO: Rename XxxApi in this file to XxxTreeType or XxxTreeTerm
    protected trait TupleTreeType extends TupleTypeApi with ValueTreeType {
      override def hashCode(): Int = ???

      override def equals(that: scala.Any): Boolean = ???

      def term(tree: Tree) = ???

    }

    type TupleType <: (ValueType with Any) with TupleTreeType

    final case class TupleParameter[ElementType <: ValueType](id: Any, elementType: ElementType, length: Int)
        extends Tree
        with Parameter {
      type TermIn[C <: Category] = C#TupleTerm {
        type Element = elementType.TermIn[C]
      }

      def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean = {
        map.get(this) match {
          case null =>
            that match {
              case TupleParameter(thatId, thatElementType, thatLength)
                  if elementType == thatElementType && length == thatLength =>
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
                  elementType.##
                ),
                length
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
            foreignCategory.tuple
              .parameter[elementType.TypeIn[foreignCategory.type]](id, elementType.in(foreignCategory), length))
          .asInstanceOf[TermIn[foreignCategory.type]]
      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = {
          val newId = new AnyRef {
            override val toString: String = raw"""α-converted(${id.toString})"""
          }
          TupleParameter[ElementType](newId, elementType, length)
        }
        context.asScala.getOrElseUpdate(this, converted)

      }
    }

    final case class Apply[Element0 <: ValueTerm](tuple: Tree {
      type TermIn[C <: Category] = C#TupleTerm { type Element = Element0#TermIn[C] }
    }, index: Int)
        extends Operator {
      type TermIn[C <: Category] = Element0#TermIn[C]

      def export(foreignCategory: Category, context: ExportContext): Element0#TermIn[foreignCategory.type] = {
        context.asScala
          .getOrElseUpdate(this, tuple.export(foreignCategory, context).split.apply(index))
          .asInstanceOf[Element0#TermIn[foreignCategory.type]]

      }

      def alphaConversion(context: AlphaConversionContext): Tree = {
        def converted = {
          copy(
            tuple = tuple
              .alphaConversion(context)
              .asInstanceOf[Tree {
                type TermIn[C <: Category] = C#TupleTerm { type Element = Element0#TermIn[C] }
              }])
        }
        context.asScala.getOrElseUpdate(this, converted)

      }
    }

    protected trait TupleTreeTerm extends ValueTreeTerm with TupleTermApi {
      thisTuple: TupleTerm =>

      def valueType = ???

      val length: Int

      val elementType: ValueType

      def split: Seq[Element] = {
        new IndexedSeq[Element] {
          def length = thisTuple.length
          def apply(index: Int): Element = {
            elementType.term(Apply(tree, index)).asInstanceOf[Element]
          }
        }
      }
    }

    type TupleTerm <: (ValueTerm with Any) with TupleTreeTerm

    @inject
    protected def tupleTermFactory[LocalElement <: ValueTerm]: Factory3[ValueType,
                                                                        Int,
                                                                        Tree,
                                                                        TupleTerm {
                                                                          type Element = LocalElement
                                                                        }]

    protected trait TreeTupleSingleton extends TupleSingletonApi {

      def parameter[ElementType <: ValueType](id: Any, elementType0: ElementType, length: Int): TupleTerm {
        type Element = elementType0.ThisTerm
      } = {
        val parameterTree = TupleParameter[ElementType](id, elementType0, length)
        tupleTermFactory[elementType0.ThisTerm].newInstance(elementType0, length, parameterTree)
      }

      def concatenate[Element0 <: ValueTerm](elements: Element0*): TupleTerm { type Element = Element0 } = {
        ???
      }

    }

    type TupleSingleton <: TreeTupleSingleton

  }

  trait AllTrees extends TupleTrees with FloatTrees with ArrayTrees with AllExpressions

}
