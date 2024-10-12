package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom.:=>

object Circuits {

  final lazy val fn0: Int :=> Int = :=> { v =>
    v + 1
  }

  final lazy val fn1: Int :=> Seq[Long] = :=> { v =>
    Seq(v, v + 1, v + 2)
  }

  final lazy val fn2: Long :=> Seq[Double] = :=> { v =>
    Seq(v, v * 0.1, v * 0.2)
  }

  lazy val fn0Text: String = {
    fn0.explain.nodeText
  }

}
