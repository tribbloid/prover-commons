package ai.acyclic.prover.commons.compat

import ai.acyclic.prover.commons.compat.TupleX.{*:, T0}

trait TupleXOrdering {

  trait Impl[T <: TupleX] extends Ordering[T]
}

object TupleXOrdering {

  // from shapeless.ops.hlists
  object Native extends TupleXOrdering {

    implicit object empty extends Impl[T0] {
      def compare(x: T0, y: T0): Int = 0
    }

    implicit def others[H, T <: TupleX](
        implicit
        hOrdering: Ordering[H],
        tOrdering: Impl[T]
    ): Impl[H *: T] =
      new Impl[H *: T] {
        def compare(x: H *: T, y: H *: T): Int = {
          val compareH = hOrdering.compare(x.head, y.head)

          if (compareH != 0) compareH else tOrdering.compare(x.tail, y.tail)
        }
      }

  }
}
