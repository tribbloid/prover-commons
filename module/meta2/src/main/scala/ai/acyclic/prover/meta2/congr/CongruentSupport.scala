package ai.acyclic.prover.meta2.congr

trait CongruentSupport {

  implicit def implies[X, Y](
      implicit
      c: Congruent[X, Y]
  ): X =:= Y = c.equality

  implicit def singletonImplies[X <: Singleton, Y <: Singleton](
      implicit
      ev: X =:= Y
  ): Congruent[X, Y] = new Congruent[X, Y](ev)
}

object CongruentSupport {}
