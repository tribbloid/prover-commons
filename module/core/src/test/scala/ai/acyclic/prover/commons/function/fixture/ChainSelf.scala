package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>

object ChainSelf {

  import Fns._

  val s1 = fn0.andThen[Int] {
    fn0
  }

  val s2 = for (i <- :=>.Identity[Int]().out) yield {

    fn0.^(fn0.^(i))
  }

  lazy val pairs = {

//
//    :=>.at[Int].trace { v =>
//      fn0.^(fn0.^(v))
//    }

    //    val s2 = {
    //
    //      fn0.^.apply(fn0) // TODO: I don't need this, clean up
    //    }

    val str = s"""
                 |+ AndThen
                 |!-- ${fn0.explain.nodeText}
                 |!-- ${fn0.explain.nodeText}
                 |""".stripMargin

    val pairs: Seq[
      (Int :=> Int, String)
    ] = Seq(
      (s1, str),
      (s2, str)
//      (s3, str)
    )

    pairs
  }

}
