package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.Symbolic.{:=>, :|~>}

trait MorphismFixture {

  lazy val _morphism: List :|~> Option = new (List :|~> Option) {

    override def specific[T]: List[T] :=> Option[T] = { v =>
      v.headOption
    }
  }
}
