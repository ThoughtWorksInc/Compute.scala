package com.thoughtworks.compute

import com.thoughtworks.feature.Factory.inject
import com.thoughtworks.feature.{Factory, ImplicitApply}
import com.thoughtworks.compute.NDimensionalAffineTransform.MatrixData

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Expressions {
  type Category >: this.type <: Expressions

  protected trait ExpressionApi {
    type TermIn[C <: Category] <: C#Term

    type ThisTerm = TermIn[Expressions.this.type]

  }

  protected trait TermApi extends ExpressionApi { this: Term =>

  }

  type Term <: TermApi

  protected trait TypeApi extends ExpressionApi

  type Type <: TypeApi

}

object Expressions {

  trait Anonymous extends Any

  object Anonymous {

    implicit def implicitValue[A, Constructor, ImplicitApplied](
        implicit factory: Factory.Aux[(A with Anonymous), Constructor],
        implicitApply: ImplicitApply.Aux[Constructor, ImplicitApplied],
        asImplicitValue: ImplicitApplied <:< (A with Anonymous)
    ): Implicitly[A] = {
      asImplicitValue(implicitApply(factory.newInstance))
    }

  }
  type Implicitly[A] = A with Anonymous

  /**
    * @author 杨博 (Yang Bo)
    */
  trait Values extends Expressions {
    type Category >: this.type <: Values

    protected trait ValueExpressionApi extends ExpressionApi { thisValue =>

      type JvmValue
      type TermIn[C <: Category] <: C#ValueTerm
      type TypeIn[C <: Category] <: C#ValueType {
        type JvmValue = thisValue.JvmValue
      }
      type ThisType = TypeIn[Values.this.type]
    }

    protected trait ValueTermApi extends TermApi with ValueExpressionApi { this: ValueTerm =>
    }

    /** @template */
    type ValueTerm <: (Term with Any) with ValueTermApi

    protected trait ValueTypeApi extends ValueExpressionApi {

      def literal(value: JvmValue): ThisTerm

      def parameter(id: Any): ThisTerm

    }

    type ValueType <: (Type with Any) with ValueTypeApi
  }

  // TODO: Boolean types

  /**
    * @author 杨博 (Yang Bo)
    */
  trait Floats extends Values {
    type Category >: this.type <: Floats

    protected trait FloatExpressionApi extends ValueExpressionApi {
      type JvmValue = Float
      type TermIn[C <: Category] = C#FloatTerm
      type TypeIn[C <: Category] = C#FloatType
    }

    protected trait FloatTermApi extends ValueTermApi with FloatExpressionApi { this: FloatTerm =>
      def +(rightHandSide: FloatTerm): FloatTerm
      def -(rightHandSide: FloatTerm): FloatTerm
      def *(rightHandSide: FloatTerm): FloatTerm
      def /(rightHandSide: FloatTerm): FloatTerm
      def %(rightHandSide: FloatTerm): FloatTerm
      def unary_- : FloatTerm
      def unary_+ : FloatTerm
    }

    type FloatTerm <: (ValueTerm with Any) with FloatTermApi

    protected trait FloatTypeApi extends ValueTypeApi with FloatExpressionApi {}

    type FloatType <: (ValueType with Any) with FloatTypeApi

    @inject
    val float: Implicitly[FloatType]

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  trait Arrays extends Values {
    type Category >: this.type <: Arrays

    protected trait ValueTermApi extends super.ValueTermApi { thisValue: ValueTerm =>

      // TODO: Remove this method
      def fill: ArrayTerm {
        type Element = thisValue.ThisTerm
      }
    }

    override type ValueTerm <: (Term with Any) with ValueTermApi

    protected trait ArrayTermApi extends TermApi { thisArray: ArrayTerm =>
      type TermIn[C <: Category] = C#ArrayTerm {
        type Element = thisArray.Element#TermIn[C]
      }

      type Element <: ValueTerm

      def extract: Element

      def transform(matrix: MatrixData): ThisTerm
    }

    type ArrayTerm <: (Term with Any) with ArrayTermApi

    @inject
    val array: Implicitly[ArrayCompanion]

    protected trait ArrayCompanionApi {

      def parameter[Element0 <: ValueTerm](id: Any, padding: Element0, shape: Array[Int]): ArrayTerm {
        type Element = Element0
      }

    }

    type ArrayCompanion <: ArrayCompanionApi

  }

  /**
    * @author 杨博 (Yang Bo)
    */
  @deprecated(message = "Use [[AllExpressions]] instead", since = "0.2.0")
  trait FloatArrays extends Floats with Arrays {
    type Category >: this.type <: Floats with Arrays
  }

  trait Tuples extends Values {
    type Category >: this.type <: Tuples

    protected trait TupleExpressionApi extends ExpressionApi { thisTuple =>
      type Element <: ValueTerm
      type JvmValue = Array[ValueTerm#JvmValue]
      type TermIn[C <: Category] = C#TupleTerm {
        type Element = thisTuple.Element#TermIn[C]
      }
      type TypeIn[C <: Category] = C#TupleType {
        type Element = thisTuple.Element#TermIn[C]
        type JvmValue = thisTuple.JvmValue
      }
    }

    protected trait TupleTermApi extends ValueTermApi with TupleExpressionApi { this: TupleTerm =>

    }
    type TupleTerm <: (ValueTerm with Any) with TupleTermApi

    protected trait TupleTypeApi extends ValueTypeApi with TupleExpressionApi {}

    type TupleType <: (ValueType with Any) with TupleTypeApi

    @inject
    val tuple: Implicitly[TupleSingleton]

    protected trait TupleSingletonApi {

      def parameter[ElementType <: ValueType](id: Any, elementType: ElementType, numberOfElements: Int): TupleTerm {
        type Element = elementType.ThisTerm
      }

    }

    type TupleSingleton <: TupleSingletonApi

  }

  trait AllExpressions extends Tuples with Floats with Arrays {
    type Category >: this.type <: AllExpressions
  }

}
