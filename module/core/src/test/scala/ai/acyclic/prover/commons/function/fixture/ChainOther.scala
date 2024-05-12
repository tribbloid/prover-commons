package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.HomSystem

trait ChainOther extends Fns {

  lazy val chainOther: HomSystem.FnImpl[fn0.In, String] = fn0.andThen[String] { v: Int =>
    s"${v}b"
  }
}
