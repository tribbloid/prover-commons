package ai.acyclic.prover.commons.meta.format.beans

import ai.acyclic.prover.commons.meta.format.FormatOvrd.SingletonName
import shapeless.{::, HNil}

trait Beans {
  trait XX[T] {

    trait ZZ[TT]
  }

  trait YY[T1, T2] {}
//  object XX

  object YY

  type T1 = XX[XX[YY.type]]

  type T2 = XX[YY.type] :: XX[YY.type] :: XX[YY.type] :: HNil

  type T3 = XX[XX[YY.type]]#ZZ[YY.type]

  object Ovrd {

    type Plain = XX[XX[SingletonName[3]]]

    type Plain2 = XX[YY[SingletonName[3], SingletonName[3]]]

    type Ref = XX[XX[3]]

    type T1 = XX[SingletonName[3]] :: XX[SingletonName[3]] :: HNil
    type T2 = XX[SingletonName[3]] :: XX[3] :: HNil
    type T3 = XX[SingletonName[3]] :: XX[SingletonName[3]] :: XX[3] :: HNil
    type T4 = XX[SingletonName[3]] :: XX[SingletonName[3]] :: XX[SingletonName[3]] :: XX[3] :: HNil
  }

}

object Beans extends Beans {}
