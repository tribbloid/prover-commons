package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.fixture.Circuits.{fn1, fn2}
import ai.acyclic.prover.commons.jit.Hom.{Fn, Fn1, Fn2}
import ai.acyclic.prover.commons.jit.eval.Args.T0

object HigherOrder2 {

  // combine fn1 and fn2 using flatMap, where fn2 is a higher order element
  // Int => Seq[Double]

  val s1 = {

    val p = fn1.trace <*> fn2.trace.higherOrder
    val f = ai.acyclic.prover.commons.jit.cps.Continuation.tracingToFunction(p)

    val proto = Fn.fromFunction1 { (v: Int) =>
      val in1 = ai.acyclic.prover.commons.jit.Hom.Const.Provided(v) ><: T0
      f.apply(in1)
    }

    val result =
      for (
        case (x, fn) <- {
          proto.trace
        }
      )
        yield {

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
        |!-+ Mapped
        |: !-- Blackbox(proto <at HigherOrder2.scala:17>)
        |: !-- Blackbox(result <at HigherOrder2.scala:25>)
        |!-- Blackbox(result <at HigherOrder2.scala:24>)
        |""".stripMargin
//    s2 -> "s2"
//    s3 -> "s3",
//    s4 -> "s4"
  )

}
