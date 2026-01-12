package ai.acyclic.prover.commons.congr

trait CongruentSupport {

  given implies[X, Y](
      using
      c: X === Y
  ): (X =:= Y) = c.equality

  inline given singletonImplies[X <: Singleton, Y <: Singleton](
      using
      ev: X =:= Y
  ): (X === Y) = new Congruent[X, Y](ev)
}

object CongruentSupport {}
