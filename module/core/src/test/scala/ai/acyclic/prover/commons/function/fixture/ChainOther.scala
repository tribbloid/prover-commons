package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.HomSystem

trait ChainOther extends Fns {

  lazy val chainOther: HomSystem.FnImpl[Int, String] = fn0.andThen[String] { v =>
    s"${v}b"
  }
}
