package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom

/**
  * Polymorphic function, each instance can convert from [[from.Inductive]] to [[to.Inductive]]
  *
  * e.g.
  *   - A ><: B ><: from.Empty -> A ><: B ><: to.Empty
  *   - A ><: from.Empty -> A ><: to.Empty
  *   - from.Empty -> to.Empty
  */
trait Converter extends Hom.Poly {

  val from: BTuples
  val to: BTuples

  implicit def emptyCase: from.Empty |- to.Empty =
    at[from.Empty] { _ =>
      to.Empty
    }

  implicit def inductiveCase[
      HEAD <: from.VBound,
      TAIL <: from.Inductive,
      TO_TAIL <: to.Inductive
  ](
      implicit
      tailCase: TAIL |- TO_TAIL,
      bound: HEAD <:< to.VBound
  ): from.><:[HEAD, TAIL] |- to.><:[HEAD & to.VBound, TO_TAIL] =
    at[from.><:[HEAD, TAIL]] { v =>
      val (head, tail) = from.deCons(v)
      to.cons[HEAD & to.VBound, TO_TAIL](
        head.asInstanceOf[HEAD & to.VBound],
        tailCase(tail).asInstanceOf[TO_TAIL]
      )
    }
}
