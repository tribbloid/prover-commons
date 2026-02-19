package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom

/**
  * Polymorphic function, each instance can convert from [[from.system.Prod]] to [[to.system.Prod]]
  *
  * e.g.
  *   - from.system.Empty -> to.system.Empty
  *   - from.G[A] ><: from.system.Empty -> to.G[A] ><: to.system.Empty
  *   - from.G[A] ><: from.G[B] ><: from.system.Empty -> to.G[A] ><: to.G[B]><: to.system.Empty
  *   - ...
  */
trait Transformation extends Hom.Poly {
  import Transformation.*

  val from: Schema
  val to: Schema

  implicit def emptyCase: from.system.Eye /=> to.system.Eye

  def pointwise[HEAD](v: from.system.Element[from.G[HEAD]]): to.system.Element[to.G[HEAD]]

  implicit def inductiveCase[
      HEAD,
      TAIL <: from.system.Prod,
      TO_TAIL <: to.system.Prod
  ](
      implicit
      tailCase: TAIL /=> TO_TAIL
  ): from.system.><:[from.G[HEAD], TAIL] /=> to.system.><:[to.G[HEAD], TO_TAIL] =
    at[from.system.><:[from.G[HEAD], TAIL]] { v =>
      val (head, tail) = from.system.deCons(v)
      val _head = pointwise(head)
      to.system.cons(_head, tailCase(tail))
    }
}

object Transformation {

  trait Schema {

    val system: Products.Monoidal
    type G[T] <: system.VBound

    type E[T] = system.Element[G[HEAD]]
  }
}
