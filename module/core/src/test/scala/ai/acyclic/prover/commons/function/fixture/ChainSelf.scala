package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>
import ai.acyclic.prover.commons.function.{Hom, Impl}
import ai.acyclic.prover.commons.function.hom.HomSystem.Fn

trait ChainSelf extends Fns {

  lazy val chainSelf = {

    val s1 = fn0.andThen[Int] {
      fn0

//      val repr = HomSystem.FnOps(fn0)
//      repr
    }

    val s2 = {
      fn0.^(fn0)
    }

    val s3 = Hom.at[Int].trace { v =>
      fn0.^(fn0.^(v))
    }

    val str = s"""
                 |+ AndThen
                 |!-- ${fn0Text}
                 |!-- ${fn0Text}
                 |""".stripMargin

    val pairs: Seq[
      (Int :=> Int, String)
    ] = Seq(
      (s1, str),
      (s2, str),
      (s3, str)
    )

    pairs
  }

}
