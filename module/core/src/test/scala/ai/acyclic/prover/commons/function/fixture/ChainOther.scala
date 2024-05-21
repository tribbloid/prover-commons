package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.System

object ChainOther {

  import Fns._

  lazy val s1: System.FnImpl[Int, String] = fn0.andThen[String] { v =>
    s"${v}b"
  }
}
