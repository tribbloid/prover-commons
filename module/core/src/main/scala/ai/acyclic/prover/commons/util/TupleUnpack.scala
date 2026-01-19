package ai.acyclic.prover.commons.util

import shapeless.*
import shapeless.ops.hlist.{IsHCons, Tupler}

trait TupleUnpack[T] {
  type Head
  type Tail

  def unpack(t: T): (Head, Tail)
  def pack(h: Head, t: Tail): T // same as Zippable
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
    def pack(h: T, t: Unit): T = h
  }
}

object TupleUnpack extends TupleUnpack_Imp0 {

  implicit def tupleCase[P <: Product, L <: HList, H, T <: HList, TP](
      implicit
      gen: Generic.Aux[P, L],
      isHCons: IsHCons.Aux[L, H, T],
      tupler: Tupler.Aux[T, TP],
      genTP: Generic.Aux[TP, T]
  ): Aux[P, H, TP] = new TupleUnpack[P] {
    type Head = H
    type Tail = TP

    def unpack(t: P): (H, TP) = {
      val l = gen.to(t)
      val h = isHCons.head(l)
      val tail = isHCons.tail(l)
      (h, tupler(tail))
    }

    def pack(h: H, t: TP): P = {
      val tail = genTP.to(t)
      val l = isHCons.cons(h, tail)
      gen.from(l)
    }
  }
}

trait TupleCons[H, T] {
  type Out

  def pack(h: H, t: T): Out
}

trait TupleCons_Imp0 {

  implicit def binaryCase[H, T]: TupleCons[H, T] { type Out = (H, T) } = new TupleCons[H, T] {
    type Out = (H, T)
    def pack(h: H, t: T): (H, T) = (h, t)
  }
}

object TupleCons extends TupleCons_Imp0 {

  type Aux[H, T, O] = TupleCons[H, T] { type Out = O }

  implicit def atomCase[H]: Aux[H, Unit, H] = new TupleCons[H, Unit] {
    type Out = H
    def pack(h: H, t: Unit): H = h
  }

  implicit def tupleCase[H, T <: HList, TP <: Product, P](
      implicit
      genTP: Generic.Aux[TP, T],
      tupler: Tupler.Aux[H :: T, P]
  ): Aux[H, TP, P] = new TupleCons[H, TP] {
    type Out = P

    def pack(h: H, t: TP): P = {
      val tailHList = genTP.to(t)
      val l = h :: tailHList
      tupler(l)
    }
  }
}
