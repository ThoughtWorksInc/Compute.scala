package com.thoughtworks.expressions.api

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory.inject
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Floats extends Values {
  type Category >: this.type <: Floats

  protected trait FloatApi extends ValueApi { this: FloatTerm =>
    type ForeignTerm[C <: Category] <: C#FloatTerm

    @inject
    def xxx[C <: Category]: ForeignTerm[C] =:= Self#ForeignTerm[C]

  }

  type FloatTerm <: (ValueTerm with Any) with FloatApi

  protected trait FloatCompanionApi extends ValueCompanionApi {
    type JvmValue = Float
    type CompanionTerm = FloatTerm
  }

  type FloatCompanion <: FloatCompanionApi

  @inject
  val FloatTerm: Implicitly[FloatCompanion]

}
