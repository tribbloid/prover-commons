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

  implicit def shortcut[P <: source.system.Prod](
      implicit
      ev: source.type =:= target.type
  ): P /=> P = at { v =>
    v
  }

  implicit def emptyCase: source.system.Eye /=> target.system.Eye = at { _ =>
    target.system.Eye
  }

  def pointwise[HEAD](v: source.E[HEAD]): target.E[HEAD]

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

    type E[T] = system.Element[G[T]]
  }
}
