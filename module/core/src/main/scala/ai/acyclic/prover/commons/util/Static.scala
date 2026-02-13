package ai.acyclic.prover.commons.util

/**
  * similar to Singleton, but all instances that hs the same type signature should be the same object, not necessarily
  * globally unique
  *
  * e.g. for trait XX[T] extends Static, all (x: XX[Int]) should be the same object (as in denotational equality, to be
  * used in Congruence lemma)
  *
  * object creation should be interned
  */

sealed trait Static

object Static {

  trait Def {

    trait Impl extends Static {}

    def get[T <: Impl]: T

    final def apply[T <: Impl](): T = get[T]
  }
}
