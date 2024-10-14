//package ai.acyclic.prover.commons.function.fixture
//
//import ai.acyclic.prover.commons.function.hom.Hom.:=>
//
//object HigherOrder4 {
//
//  import Circuits._
//
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
//
//  val s2: Int :=> Seq[Double] = {
//
//    fn1.andThen {
//
//      val result: Seq[Long] :=> Seq[Double] = for (ff <- fn2.trace) yield {
//
//        :=>.at[Seq[Long]] { v =>
//          v.flatMap(ff.asScala)
//        }
//      }
//
//      result
//    }
//  }
//
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
//
//  val pairs = Seq(
//    s1 -> "s1",
//    s2 -> "s2",
//    s3 -> "s3",
//    s4 -> "s4"
//  )
//}
