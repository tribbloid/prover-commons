package ai.acyclic.prover.commons.jit

object __TheDualityOfBoundAndEvidence {

  import ai.acyclic.prover.commons.<:<

  // the following 2 functions behave the same:

  trait T1
  def fn1[T <: T1](t: T): T = ???

  def fn2[T](t: T)(
      implicit
      ev: T <:< T1
  ): T & T1 = ???

  // we should allow them to override each other:

  locally {
    def fn1[T <: T1](t: T): T & T1 = fn2[T](t) // trivial
  }

  locally {
    def fn2[T](t: T)(
        implicit
        ev: T <:< T1
    ): T & T1 = {
      val t1: T & T1 = ev(t)
      fn1(t1)
    }
  }

  // unforunately in Scala 2 the union type is missing (see "TODO: need union type" in <:<)
}
