package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.HomSystem

trait ChainOther extends Fns {

  lazy val chainOther: HomSystem.Fn.AndThen[fn0.In, fn0.Out, String] = fn0.andThen[String] { v: Int =>
    s"${v}b"
  }
}
