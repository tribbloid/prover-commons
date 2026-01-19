package ai.acyclic.prover.commons.util

import shapeless.*
import shapeless.ops.hlist.{IsHCons, Tupler}

trait TupleUnpack[T] {
  type Head
  type Tail

  def unpack(t: T): (Head, Tail)
}

trait TupleUnpack_Imp0 {

  type Aux[T, H, L] = TupleUnpack[T] {
    type Head = H
    type Tail = L
  }

  implicit def atomCase[T]: Aux[T, T, Unit] = new TupleUnpack[T] {
    type Head = T
    type Tail = Unit

    def unpack(t: T): (T, Unit) = (t, ())
  }
}

object TupleUnpack extends TupleUnpack_Imp0 {

  implicit def tupleCase[P <: Product, L <: HList, H, T <: HList, TP](
      implicit
      gen: Generic.Aux[P, L],
      isHCons: IsHCons.Aux[L, H, T],
      tupler: Tupler.Aux[T, TP]
  ): Aux[P, H, TP] = new TupleUnpack[P] {
    type Head = H
    type Tail = TP

    def unpack(t: P): (H, TP) = {
      val l = gen.to(t)
      val h = isHCons.head(l)
      val tail = isHCons.tail(l)
      (h, tupler(tail))
    }
  }
}
