package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.fixture.Circuits.{fn1, fn2}
import ai.acyclic.prover.commons.function.hom.Hom

object HigherOrder2 {

  // combine fn1 and fn2 using flatMap, where fn2 is a higher order element
  // Int => Seq[Double]

  private val pointwise = {

    Hom.Circuit
      .id[Int]
      .CrossUnit
      .andThen(
        fn1.trace >< fn2.trace.higher
      )

  }

  val s1 = for (case (o1, ff) <- pointwise) yield {}

//  val s1: Int :=> Seq[Double] = {
//    fn1.andThen {
//
//      val result: Seq[Long] :=> Seq[Double] = :=>.at[Long :=> Seq[Double]] { _fn2 =>
//        :=>.at[Seq[Long]] { o1 =>
//          o1.flatMap(_fn2)
//        }
//      }
//        .apply(fn2)
//
//      result
//    }
//  }

//  val s2: Int :=> Seq[Double] = {
//
//    fn1.andThen {
//
////      val result: Seq[Long] :=> Seq[Double] = for (ff <- fn2.trace) yield {
////
////        :=>.at[Seq[Long]] { v =>
////          v
////        }
////      }
////
////      result
//      ???
//    }
//  }

//  val s3: Int :=> Seq[Double] = {
//    for (
//      o1 <- fn1.out;
//      ff <- fn2.^
//    ) yield {
//
//      val result = o1.flatMap(ff.asScala)
//      result
//    }
//  }
//
//  val s4: Int :=> Seq[Double] = {
//    for (
//      ff <- fn2.^;
//      o1 <- fn1.out
//    ) yield {
//
//      o1.flatMap(ff.asScala)
//    }
//  }

//  val pairs = Seq(
//    s1 -> "s1",
//    s2 -> "s2"
////    s3 -> "s3",
////    s4 -> "s4"
//  )

}
