package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait FlatReprMixin {
  self: BTuples =>

  import self._

  trait ToHList[I <: self.Inductive] {
    type Out <: HList
    def apply(i: I): Out
  }

  object ToHList {
    type Aux[I <: self.Inductive, O <: HList] = ToHList[I] { type Out = O }

    implicit val empty: Aux[self.Empty, HNil] = new ToHList[self.Empty] {
      type Out = HNil
      def apply(i: self.Empty): HNil = HNil
    }

    implicit def cons[H <: self.VBound, T <: self.Inductive, TO <: HList](
        implicit
        tail: Aux[T, TO]
    ): Aux[self.><:[H, T], H :: TO] = new ToHList[self.><:[H, T]] {
      type Out = H :: TO
      def apply(i: self.><:[H, T]): H :: TO = {
        val (h, t) = self.deCons(i)
        h :: tail(t)
      }
    }
  }

  trait ToFlatRepr_Imp0 extends Poly {

    implicit def forTuples[I <: self.Inductive, L <: HList, O](
        implicit
        toHList: ToTuple.:=>[I, L],
        hlistToFlat: Tupler.Aux[L, O]
    ): I |- O = at[I] { i =>
      hlistToFlat(toHList(i))
    }
  }

  /**
    * can convert a [[Inductive]] to a flat Scala tuple or Unit or value and back
    *
    * e.g.
    *   - (A, B) <-> A ><: B ><: Empty
    *   - (A) <-> A ><: Empty
    *   - A -> A ><: Empty
    *   - Unit -> Empty
    */
  object ToFlatRepr extends ToFlatRepr_Imp0 {

    implicit lazy val forUnit: Empty |- Unit = at[Empty](_ => ())

    implicit def forValue[V <: VBound]: (V ><: Empty) |- V = at[V ><: Empty] { v =>
      val (head, _) = self.deCons(v)
      head
    }

  }

  /**
    * The inverse of [[ToFlatRepr]]
    */
  object FromFlatRepr extends Poly with FromFlatRepr_LowPriority {

    implicit val forUnit: Unit |- Empty = at[Unit](_ => self.Empty)

    implicit def forProduct[P <: Product, L <: HList, O <: Inductive](
        implicit
        gen: Generic.Aux[P, L],
        fromTuple: FromTuple.|-[L, O]
    ): P |- O = at[P] { p =>
      fromTuple(gen.to(p))
    }
  }

  trait FromFlatRepr_LowPriority {
    poly: Poly =>

    implicit def forValue[V <: VBound]: V |- (V ><: Empty) = at[V] { v =>
      self.cons(v, self.Empty)
    }
  }

}

object FlatReprMixin {}
