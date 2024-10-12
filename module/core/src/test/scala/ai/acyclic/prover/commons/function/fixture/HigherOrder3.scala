package ai.acyclic.prover.commons.function.fixture

object HigherOrder3 {

  import Circuits._

  val s1 = {

    for (case (o1, o2) <- (fn1.trace >< fn2).trace)
      yield {

        o1 -> o2
      }
  }

}
