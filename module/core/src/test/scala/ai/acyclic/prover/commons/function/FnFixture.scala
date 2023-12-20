package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.Symbolic.:=>

trait FnFixture {

  // Single Abstract Method definition
  lazy val _fn0: Int :=> String = { v =>
    val _ = v._1 // uses implicits

    val tt: Tuple1[Int] = v.asTuple // use dynamic selector

    "" + tt._1
  }

  lazy val _fn1: Int :=> Int = { v =>
    v.value1 + 1
  }

}
