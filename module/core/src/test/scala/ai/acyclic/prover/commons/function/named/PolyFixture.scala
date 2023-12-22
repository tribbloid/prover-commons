package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.function.PreDef.Named.{:=>, Poly1}

trait PolyFixture {

  object _poly extends Poly1 {

    implicit def int: Case[Int :=> Int] = {

      val at1 = forCase[Int :=> Int]
      at1.=>>(v => v.unbox + 1)
    }

    implicit def str: Case[String :=> String] = {
      forCase[String :=> _](v => v.unbox + "1")
    }
  }
}
