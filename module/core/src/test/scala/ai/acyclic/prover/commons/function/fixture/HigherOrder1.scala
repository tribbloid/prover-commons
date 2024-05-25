package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>

object HigherOrder1 {

  import Fns._

  val s1: Seq[Long] :=> Seq[Double] = {
    :=>.at[Long :=> Seq[Double]] { ff =>
      :=>.at[Seq[Long]] { o1 =>
        o1.flatMap(ff)
      }
    }
      .apply(fn2)
  }

  val s2: Seq[Long] :=> Seq[Double] = {
    for (ff <- fn2.^) yield {

      :=>.at[Seq[Long]] { v =>
        v.flatMap(ff.asScala)
      }
    }
  }

  val pairs: Seq[(Seq[Long] :=> Seq[Double], String)] =
    Seq(
      s1 -> "s1",
      s2 -> "s2"
    )
}
