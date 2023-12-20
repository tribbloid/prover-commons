package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.Symbolic.{:=>, Poly1}

trait PolyFixture {

  object _poly extends Poly1 {

    implicit def int: Case[Int :=> Int] = {

      val at1 = at[Int :=> Int]
      at1.apply(v => v.value1 + 1)
    }

    implicit def str: Case[String :=> String] = {
      at[String :=> _](v => v.value1 + "1")
    }
  }
}
