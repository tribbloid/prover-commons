package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>
import ai.acyclic.prover.commons.function.hom.System.TracerImpl

object ChainOther {

  import Fns._

  val s1: Int :=> String = fn0.andThen[String] { v =>
    s"${v}b"
  }

  val debug: TracerImpl[Seq[Long]] = fn0.^.flatMap { f1 =>
    val vv = fn1.^.map { f2 =>
      f2(f1(3))
    }
    vv
  }
}
