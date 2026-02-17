package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.multiverse.rewrite.HasCoercion

object Casting extends HasCoercion {

  // TODO: cyclic <:< should sometimes entail an equality =:=, in other time it should be a compilation error

  case class CanCast[A, B]() extends Coercion[A, A & B] {

    def fwd(v: A): A & B = v.asInstanceOf[A & B]

    // TODO: need union type
    //    def rev(v: B): B | A = ???

    override def normalise(v: A): A & B = fwd(v)
  }

  type <:<[A, +B] = CanCast[A, ? <: B]

  type >:>[+B, A] = A <:< B

  implicit def lemma[A, B](
      implicit
      ev: scala.<:<[A, B]
  ): A <:< B = new CanCast[A, B]()

}
