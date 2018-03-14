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
      case childSeq: Seq[_] =>
        seqHashCode(childSeq, context)
      case _ =>
        child.##
    }
  }

  private def seqHashCode(childSeq: Seq[_], context: HashCodeContext): Int = {
    val iterator = childSeq.iterator
    @tailrec
    def seqLoop(h: Int, i: Int): Int = {
      if (iterator.hasNext) {
        seqLoop(h = MurmurHash3.mix(h, childHashCode(iterator.next(), context)), i = i + 1)
      } else {
        MurmurHash3.finalizeHash(h, i)
      }
    }
    seqLoop(MurmurHash3.seqSeed, 0)
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

  /** @group AST */
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
        case left: Seq[_] =>
          right match {
            case right: Seq[_] =>
              val leftIterator = left.iterator
              val rightIterator = right.iterator
              @tailrec def seqLoop(): Boolean = {
                if (leftIterator.hasNext) {
                  if (rightIterator.hasNext) {
                    if (isSameChild(leftIterator.next(), rightIterator.next(), map)) {
                      seqLoop()
                    } else {
                      false
                    }
                  } else {
                    false
                  }
                } else {
                  if (rightIterator.hasNext) {
                    false
                  } else {
                    true
                  }
                }
              }
              seqLoop()
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

  /** @group AST */
  trait Parameter extends Tree { thisParameter =>

    val id: Any

  }

  final class HashCodeContext extends IdentityHashMap[Tree, Int] {
    var numberOfParameters = 0
  }

  final class AlphaConversionContext extends IdentityHashMap[Tree, Tree] {
    var seed: Int = 0
  }

  /** @group AST */
  trait Tree extends Product { thisTree =>
    type TermIn[C <: Category]

    protected def erasedExport(foreignCategory: Category, context: ExportContext): Category#Term

    final def export(foreignCategory: Category, context: ExportContext): TermIn[foreignCategory.type] = {
      erasedExport(foreignCategory, context).asInstanceOf[TermIn[foreignCategory.type]]
    }

    def isSameStructure(that: Tree, map: StructuralComparisonContext): Boolean

    def structuralHashCode(context: HashCodeContext): Int

    protected def erasedAlphaConversion(context: AlphaConversionContext): Tree

    final def alphaConversion(context: AlphaConversionContext): Tree {
      type TermIn[C <: Category] = thisTree.TermIn[C]
    } = {
      erasedAlphaConversion(context).asInstanceOf[Tree {
        type TermIn[C <: Category] = thisTree.TermIn[C]
      }]
    }
  }

  final class ExportContext extends IdentityHashMap[Tree, Category#Term]

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

      @transient
      override lazy val hashCode: Int = {
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

    /** @group AST */
    protected type FloatTree = Tree {
      type TermIn[C <: Category] = C#FloatTerm
    }

    /** @group AST */
    protected trait FloatOperator extends Operator {
      type TermIn[C <: Category] = C#FloatTerm

    }

    /** @group AST */
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

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala.getOrElseUpdate(this, foreignCategory.float.parameter(id))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(
          id = new AnyRef {
            override val toString: String = raw"""α-converted(${id.toString})"""
          }
        )
        context.asScala.getOrElseUpdate(this, converted)
      }

    }

    /** @group AST */
    @(silent @companionObject)
    final case class FloatLiteral(value: Float) extends FloatOperator {

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = this

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala.getOrElseUpdate(this, foreignCategory.float.literal(value))
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Exp(operand0: FloatTree) extends FloatOperator {

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala.getOrElseUpdate(this, foreignCategory.float.exp(operand0.export(foreignCategory, context)))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Log(operand0: FloatTree) extends FloatOperator {

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala.getOrElseUpdate(this, foreignCategory.float.log(operand0.export(foreignCategory, context)))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Abs(operand0: FloatTree) extends FloatOperator {

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala.getOrElseUpdate(this, foreignCategory.float.abs(operand0.export(foreignCategory, context)))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Min(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this,
                           foreignCategory.float.min(operand0.export(foreignCategory, context),
                                                     operand1.export(foreignCategory, context)))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Max(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this,
                           foreignCategory.float.max(operand0.export(foreignCategory, context),
                                                     operand1.export(foreignCategory, context)))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Plus(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) + operand1.export(foreignCategory, context))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Minus(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) - operand1.export(foreignCategory, context))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Times(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) * operand1.export(foreignCategory, context))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Div(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) / operand1.export(foreignCategory, context))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Percent(operand0: FloatTree, operand1: FloatTree) extends FloatOperator {
      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, operand0.export(foreignCategory, context) % operand1.export(foreignCategory, context))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand0 = operand0.alphaConversion(context), operand1 = operand1.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class UnaryMinus(operand: FloatTree) extends FloatOperator {
      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, operand.export(foreignCategory, context).unary_-)
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand = operand.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class UnaryPlus(operand: FloatTree) extends FloatOperator {
      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala.getOrElseUpdate(this, operand.export(foreignCategory, context).unary_+)
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(operand = operand.alphaConversion(context))
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

      @inline
      def min(leftHandSide: FloatTerm, rightHandSide: FloatTerm): FloatTerm = {
        term(Min(leftHandSide.tree, rightHandSide.tree))
      }

      @inline
      def max(leftHandSide: FloatTerm, rightHandSide: FloatTerm): FloatTerm = {
        term(Max(leftHandSide.tree, rightHandSide.tree))
      }
      @inline
      def log(operand: FloatTerm): FloatTerm = {
        term(Log(operand.tree))
      }
      @inline
      def exp(operand: FloatTerm): FloatTerm = {
        term(Exp(operand.tree))
      }
      @inline
      def abs(operand: FloatTerm): FloatTerm = {
        term(Abs(operand.tree))
      }

    }

    type FloatType <: (ValueType with Any) with FloatTreeType

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait ArrayTrees extends Arrays with ValueTrees {

    /** @group AST */
    type ArrayTree[LocalElement <: ValueTerm] = Tree {
      type TermIn[C <: Category] = C#ArrayTerm { type Element = LocalElement#TermIn[C] }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Extract[LocalElement <: ValueTerm](array: ArrayTree[LocalElement]) extends Operator {

      protected def erasedExport(foreignCategory: Category, map: ExportContext) = {
        map.asScala.getOrElseUpdate(this, array.export(foreignCategory, map).extract)
      }

      type TermIn[C <: Category] = LocalElement#TermIn[C]

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(array = array.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Transform[LocalElement <: ValueTerm](array: ArrayTree[LocalElement], matrix: MatrixData)
        extends Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      protected def erasedExport(foreignCategory: Category, map: ExportContext) = {
        map.asScala.getOrElseUpdate(this, array.export(foreignCategory, map).transform(matrix))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(array = array.alphaConversion(context))
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

    /** @group AST */
    @(silent @companionObject)
    final case class Fill[LocalElement <: ValueTerm](element: Tree {
      type TermIn[C <: Category] = LocalElement#TermIn[C]
    }) extends Operator {
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = LocalElement#TermIn[C]
      }

      protected def erasedExport(foreignCategory: Category, map: ExportContext) = {
        map.asScala.getOrElseUpdate(this, element.export(foreignCategory, map).fill)
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy[LocalElement](element = element.alphaConversion(context))
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

    /** @group AST */
    @(silent @companionObject)
    final case class ArrayParameter[LocalElement <: ValueTerm](id: Any, padding: Tree {
      type TermIn[C <: Category] = LocalElement#TermIn[C]
    }, shape: Array[Int])
        extends Parameter {
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

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        def foreignTerm = {
          foreignCategory.array.parameter(id, padding.export(foreignCategory, context), shape)
        }
        context.asScala.getOrElseUpdate(this, foreignTerm)
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy[LocalElement](
          padding = padding
            .alphaConversion(context)
            .asInstanceOf[Tree {
              type TermIn[C <: Category] = LocalElement#TermIn[C]
            }],
          id = {
            val id = context.seed
            context.seed = id + 1
            id
          }
        )

        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    @inject
    protected def arrayTermFactory[LocalElement <: ValueTerm]: Factory2[ValueType,
                                                                        Tree,
                                                                        ArrayTerm {
                                                                          type Element = LocalElement
                                                                        }]

    protected trait TreeArraySingleton extends ArraySingletonApi {

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

    type ArraySingleton <: TreeArraySingleton
  }

  private val tupleHashSeed = "TupleType".##

  trait TupleTrees extends ValueTrees with Tuples {

    protected trait TupleTreeType extends TupleTypeApi with ValueTreeType {

      @transient
      override lazy val hashCode: Int = {
        MurmurHash3.finalizeHash(MurmurHash3.mixLast(MurmurHash3.mix(tupleHashSeed, elementType.##), length), 2)
      }

      override def equals(that: scala.Any): Boolean = {
        that match {
          case that: TupleTreeType =>
            this.elementType.equals(that.elementType) && this.length.equals(that.length)
          case _ =>
            false
        }
      }

      val elementType: ValueType

      val length: Int

      def term(tree: Tree): ThisTerm = {
        tupleTermFactory[Element].newInstance(elementType, length, tree).asInstanceOf[ThisTerm]
      }

      def in(foreignCategory: Category): TypeIn[foreignCategory.type] = {
        foreignCategory.tuple.apply(elementType.in(foreignCategory), length).asInstanceOf[TypeIn[foreignCategory.type]]
      }
    }

    type TupleType <: (ValueType with Any) with TupleTreeType

    @inject
    protected def tupleTreeTypeFactory[LocalElement <: ValueTerm]: Factory2[ValueType,
                                                                            Int,
                                                                            TupleType {
                                                                              type Element = LocalElement
                                                                            }]

    /** @group AST */
    @(silent @companionObject)
    final case class TupleParameter(id: Any, elementType: ValueType, length: Int) extends Parameter {
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

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, foreignCategory.tuple.parameter(id, elementType.in(foreignCategory), length))
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(
          id = new AnyRef {
            override val toString: String = raw"""α-converted(${id.toString})"""
          }
        )
        context.asScala.getOrElseUpdate(this, converted)
      }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Concatenate[Element0 <: ValueTerm](elementTrees: Seq[Tree {
      type TermIn[C <: Category] = Element0#TermIn[C]
    }]) extends Operator { thisTree =>
      type TermIn[C <: Category] = C#TupleTerm { type Element = Element0#TermIn[C] }

      protected def erasedExport(foreignCategory: Category, context: ExportContext): Category#Term = {
        def foreignTerm = {
          val foreignElements = elementTrees.map(_.export(foreignCategory, context))
          foreignCategory.tuple.zip(foreignElements: _*)
        }
        context.asScala.getOrElseUpdate(this, foreignTerm)
      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        val converted = copy[Element0](
          elementTrees = elementTrees.map(_.alphaConversion(context))
        )
        context.asScala.getOrElseUpdate(this, converted)

      }
    }

    /** @group AST */
    type TupleTree[LocalElement <: ValueTerm] = Tree {
      type TermIn[C <: Category] = C#TupleTerm { type Element = LocalElement#TermIn[C] }
    }

    /** @group AST */
    @(silent @companionObject)
    final case class Apply[LocalElement <: ValueTerm](tuple: TupleTree[LocalElement], index: Int) extends Operator {
      type TermIn[C <: Category] = LocalElement#TermIn[C]

      protected def erasedExport(foreignCategory: Category, context: ExportContext) = {
        context.asScala
          .getOrElseUpdate(this, tuple.export(foreignCategory, context).unzip.apply(index))

      }

      protected def erasedAlphaConversion(context: AlphaConversionContext): Tree = {
        def converted = copy(tuple = tuple.alphaConversion(context))
        context.asScala.getOrElseUpdate(this, converted)

      }
    }

    protected trait TupleTreeTerm extends ValueTreeTerm with TupleTermApi {
      thisTuple: TupleTerm =>

      def valueType = tupleTreeTypeFactory[Element].newInstance(elementType, length).asInstanceOf[ThisType]

      val elementType: ValueType

      val length: Int

      def unzip: Seq[Element] = {
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

      def apply(element: ValueType, length: Int): TupleType { type Element = element.ThisTerm } = {
        tupleTreeTypeFactory[element.ThisTerm].newInstance(element, length)
      }

      def parameter(id: Any, element: ValueType, length: Int): TupleTerm {
        type Element = element.ThisTerm
      } = {
        val parameterTree = TupleParameter(id, element, length)
        tupleTermFactory[element.ThisTerm].newInstance(element, length, parameterTree)
      }

      def zip[Element0 <: ValueTerm](elements: Element0*): TupleTerm { type Element = Element0 } = {
        val elementTrees = elements.map(_.tree.asInstanceOf[Tree { type TermIn[C <: Category] = Element0#TermIn[C] }])
        val zipTree = Concatenate[Element0](elementTrees)
        tupleTermFactory[Element0].newInstance(elements.head.valueType, elements.length, zipTree)
      }

    }

    type TupleSingleton <: TreeTupleSingleton

  }

  trait AllTrees extends TupleTrees with FloatTrees with ArrayTrees with AllExpressions

}
