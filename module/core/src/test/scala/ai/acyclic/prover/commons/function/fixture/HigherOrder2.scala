package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.Hom

object HigherOrder2 {

  import Circuits._

  val s1 = {

    val pointwise: Hom.Circuit.Tracing[(Int, Long), (Seq[Long], Seq[Double])] = fn1.trace >< fn2.trace

    pointwise.map {
      case (o1, o2) =>
        o1 -> o2
    }

    for (case (o1, o2) <- pointwise)
      yield {

        o1 -> o2
      }
  }
}
