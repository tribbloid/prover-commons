package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.function.PreDef.Named.:=>

trait FnFixture {

  // Single Abstract Method definition
  lazy val _fn0: Int :=> String = T1.vanillaToFn { v =>
    val _ = v._1 // uses implicits

    val tt: Tuple1[Int] = v.asTuple // use dynamic selector

    "" + tt._1
  }

  lazy val _fn1: Int :=> Int = T1.vanillaToFn { v =>
    v.unbox + 1
  }

}
