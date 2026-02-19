package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom

/**
  * Polymorphic function, each instance can convert from [[source.system.Prod]] to [[target.system.Prod]]
  *
  * e.g.
  *   - from.system.Empty -> to.system.Empty
  *   - from.G[A] ><: from.system.Empty -> to.G[A] ><: to.system.Empty
  *   - from.G[A] ><: from.G[B] ><: from.system.Empty -> to.G[A] ><: to.G[B]><: to.system.Empty
  *   - ...
  */
trait Transformation extends Hom.Poly {
  import Transformation.*

  val source: Schema
  val target: Schema

  // TODO: add a shortcut for case where source == target, in which case no implicit is required

  implicit def emptyCase: source.system.Eye /=> target.system.Eye = at { _ =>
    target.system.Eye
  }

  def pointwise[HEAD](v: source.system.Element[source.G[HEAD]]): target.system.Element[target.G[HEAD]]

  implicit def inductiveCase[
      HEAD,
      TAIL <: source.system.Prod,
      TO_TAIL <: target.system.Prod
  ](
      implicit
      tailCase: TAIL /=> TO_TAIL
  ): source.system.><:[source.G[HEAD], TAIL] /=> target.system.><:[target.G[HEAD], TO_TAIL] =
    at[source.system.><:[source.G[HEAD], TAIL]] { v =>
      val (head, tail) = source.system.deCons(v)
      val _head = pointwise(head)
      target.system.cons(_head, tailCase(tail))
    }
}

object Transformation {

  trait Schema {

    val system: Products.Monoidal
    type G[T] <: system.VBound

    type E[T] = system.Element[G[T]] // TODO: this can be used to simplify this file
  }
}
