package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.hom.Hom.:=>
import ai.acyclic.prover.commons.jit.eval.Args.{><:, T0}

object Circuits {

  final lazy val fn0: (Int ><: T0) :=> Int = { (v: Int) =>
    v + 1
  }

  final lazy val fn1: (Int ><: T0) :=> Seq[Long] = { (v: Int) =>
    Seq(v.toLong, v.toLong + 1L, v.toLong + 2L)
  }

  final lazy val fn2: (Long ><: T0) :=> Seq[Double] = { (v: Long) =>
    Seq(v.toDouble, v + 0.1, v + 0.2)
  }

  lazy val fn0Text: String = {
    fn0.explain.nodeText
  }

}
