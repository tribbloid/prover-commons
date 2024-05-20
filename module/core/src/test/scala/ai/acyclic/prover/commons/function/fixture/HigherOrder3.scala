package ai.acyclic.prover.commons.function.fixture

object HigherOrder3 {

  import Fns._

  val s1 = { // currying

    for (
      o1 <- fn1.out;
      o2 <- fn2.out
    ) yield {

      o1.zip(o2)
    }
  }

}
