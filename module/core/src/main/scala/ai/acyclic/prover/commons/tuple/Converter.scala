package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom

/**
  * Polymorphic function, each instance can convert from [[from.Prod]] to [[to.Prod]]
  *
  * e.g.
  *   - A ><: B ><: from.Empty -> A ><: B ><: to.Empty
  *   - A ><: from.Empty -> A ><: to.Empty
  *   - from.Empty -> to.Empty
  */
trait Converter extends Hom.Poly {

  val from: Products.Monoidal
  val to: Products.Monoidal

  implicit def emptyCase: from.Eye |- to.Eye

  def pointwise[T <: from.VBound & to.VBound]: from.Element[T] => to.Element[T]

  implicit def inductiveCase[
      HEAD <: from.VBound & to.VBound,
      TAIL <: from.Prod,
      TO_TAIL <: to.Prod
  ](
      implicit
      tailCase: TAIL /=> TO_TAIL
  ): from.><:[HEAD, TAIL] /=> to.><:[HEAD, TO_TAIL] =
    at[from.><:[HEAD, TAIL]] { v =>
      val (head, tail) = from.deCons(v)
      val _head: to.Element[HEAD] = pointwise[HEAD](head)
      to.cons(_head, tailCase(tail))
    }
}
