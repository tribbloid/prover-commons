package ai.acyclic.prover.meta2.congr

import ai.acyclic.prover.commons.util.StaticTag

trait CongruentSupport { // TODO: merge into Congruent

  implicit def implies[X, Y](
      implicit
      c: Congruent[X, Y]
  ): X =:= Y = c.equality

  implicit def singletonImplies[X, Y](
      implicit
      xIs: CongruentSupport.IsSingletonOrStatic[X],
      yIs: CongruentSupport.IsSingletonOrStatic[Y],
      ev: X =:= Y
  ): Congruent[X, Y] = new Congruent[X, Y](ev)

}

object CongruentSupport {

  trait IsSingletonOrStatic[T]

  object IsSingletonOrStatic {

    implicit def isSingleton[T <: Singleton]: IsSingletonOrStatic[T] = null

    implicit def isStatic[T <: StaticTag]: IsSingletonOrStatic[T] = null
  }
}
