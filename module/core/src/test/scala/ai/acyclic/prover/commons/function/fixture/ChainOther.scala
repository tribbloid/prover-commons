package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.HomSystem

object ChainOther {

  import Fns._

  lazy val s1: HomSystem.FnImpl[Int, String] = fn0.andThen[String] { v =>
    s"${v}b"
  }
}
