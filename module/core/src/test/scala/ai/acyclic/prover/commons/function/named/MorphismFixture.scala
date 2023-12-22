package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.function.PreDef.Named.{:=>, :|~>}

trait MorphismFixture {

  lazy val _morphism: List :|~> Option = new (List :|~> Option) {

    override def specific[T]: List[T] :=> Option[T] = { v =>
      v.headOption
    }
  }
}
