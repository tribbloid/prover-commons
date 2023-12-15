package ai.acyclic.prover.commons.function

import shapeless.HNil

object T0 extends Tier {

  type IUB = HNil

  //  trait Fn[+R] extends Function0[R] with Function[IUB, R] {
  //
  //    def apply(): R = hForm(Args(HNil))
  //  }
}
