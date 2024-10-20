package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.fixture.Circuits.{fn1, fn2}
import ai.acyclic.prover.commons.function.hom.Hom

object HigherOrder2 {

  // combine fn1 and fn2 using flatMap, where fn2 is a higher order element
  // Int => Seq[Double]

  val s1 = {

    val proto = Hom.Circuit
      .id[Int]
      .CrossUnit
      .andThen(
        fn1.trace >< fn2.trace.higher
      )

    val result =
      for (
        case (x, fn) <- {
          proto.trace
        }
      ) yield {

        val result = x.flatMap(fn)
        result
      }

    result
  }

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

  val pairs = Seq(
    s1 ->
      s"""
        |+ Mapped
        |!-+ Filtered
        |: !-+ Mapped
        |: : !-- CrossUnit
        |: : !-+ Pointwise
        |: :   !-- ${fn1.explain.nodeText}
        |: :   !-+ Eager
        |: :     !-- ${fn2.explain.nodeText}
        |: !-- Blackbox(result <at HigherOrder2.scala:23>)
        |!-- Blackbox(result <at HigherOrder2.scala:22>)
        |""".stripMargin
//    s2 -> "s2"
//    s3 -> "s3",
//    s4 -> "s4"
  )

}
