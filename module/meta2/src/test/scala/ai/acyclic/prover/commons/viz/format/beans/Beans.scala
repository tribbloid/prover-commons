package ai.acyclic.prover.commons.viz.format.beans

import ai.acyclic.prover.commons.viz.format.FormatOvrd.SingletonName

trait Beans {
  trait XX[T] {

    trait ZZ[TT]
  }

  trait YY[T1, T2] {}
//  object XX

  object YY

  trait T0

  type T1 = XX[XX[YY.type]]

  trait ><[X, Y]

  type T2 = XX[YY.type] >< XX[YY.type] >< XX[YY.type] >< T0

  type T3 = XX[XX[YY.type]]#ZZ[YY.type]

  object Ovrd {

    type Plain = XX[XX[SingletonName[3]]]

    type Plain2 = XX[YY[SingletonName[3], SingletonName[3]]]

    type Ref = XX[XX[3]]

    type T1 = XX[SingletonName[3]] >< XX[SingletonName[3]] >< T0
    type T2 = XX[SingletonName[3]] >< XX[3] >< T0
    type T3 = XX[SingletonName[3]] >< XX[SingletonName[3]] >< XX[3] >< T0
    type T4 = XX[SingletonName[3]] >< XX[SingletonName[3]] >< XX[SingletonName[3]] >< XX[3] >< T0
  }

}

object Beans extends Beans {}
