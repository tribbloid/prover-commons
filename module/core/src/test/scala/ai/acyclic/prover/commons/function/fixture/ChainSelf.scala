package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom.:=>
import ai.acyclic.prover.commons.function.hom.System.FnImpl

object ChainSelf {

  import Fns._

  val s0: Int :=> Int = fn0.andThen(fn0)

  val s1 = FnImpl.ContinuationView(fn0.out).map(fn0)

  val s2 = :=>.id[Int].out.map(fn0).out.map(fn0)

  val s3: Int :=> Int =
    for (
      x <- :=>.id[Int].out;
      f1 <- fn0.^;
      f2 <- fn0.^
    ) yield {

      f2(f1(x))
    }

  val s4: Int :=> Int = {
    :=>.id[Int].out.flatMap { x =>
      val v = fn0.^.flatMap { f1 =>
        val vv = fn0.^.map { f2 =>
          f2(f1(x))
        }
        vv
      }
      v
    }
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
                 |+ Compose
                 |!-- ${fn0.explain.nodeText}
                 |!-- ${fn0.explain.nodeText}
                 |""".stripMargin

    val pairs: Seq[
      (Int :=> Int, String)
    ] = Seq(
      (s0, str),
      (s1, str),
      (s2, str),
      (s3, str),
      (s4, str)
    )

    pairs
  }

}
