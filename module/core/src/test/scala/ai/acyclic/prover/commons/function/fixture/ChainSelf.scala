package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>

trait ChainSelf extends Fns {

  lazy val chainSelf = {

    val s1 = fn0.andThen[Int] {
      fn0
    }

    val s2 = for (i <- :=>.Identity[Int]().out) yield {

      fn0.^(fn0.^(i))
    }
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
                 |!-- ${fn0Text}
                 |!-- ${fn0Text}
                 |""".stripMargin

    val pairs: Seq[
      (Int :=> Int, String)
    ] = Seq(
//      (s1, str)
      (s2, str)
//      (s3, str)
    )

    pairs
  }

}
