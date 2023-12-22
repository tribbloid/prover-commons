package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.PreDef.{:=>, Poly}

trait PolyFixture {

  object _poly extends Poly {

    implicit lazy val int: Case[Int :=> Int] = {

      at[Int](v => v + 1)
    }

    implicit lazy val str: Case[String :=> String] = {
      forCase[String :=> String](v => v + "1")
    }
  }
}
