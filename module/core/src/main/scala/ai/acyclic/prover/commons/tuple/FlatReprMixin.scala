package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait FlatReprMixin {
  self: BTuples =>

  import self._

  trait ToFlatRepr_Imp0 extends Poly {

    implicit def forTuples[I <: self.Inductive, L <: HList, LN <: HList, O](
        implicit
        toHList: ToTuple.|-[I, L],
        normalize: NormalizeHList.Aux[L, LN],
        hlistToFlat: Tupler.Aux[LN, O]
    ): I |- O = at[I] { i =>
      hlistToFlat(normalize(toHList(i)))
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

    implicit def forProduct[P <: Product, L <: HList, O](
        implicit
        gen: Generic.Aux[P, L],
        fromTuple: FromTuple.|-[L, O]
    ): P |- O = at[P] { p =>
      fromTuple(gen.to(p))
    }

  }

  trait NormalizeHList[L] {
    type Out <: HList
    def apply(l: L): Out
  }

  object NormalizeHList {
    type Aux[L, O <: HList] = NormalizeHList[L] { type Out = O }

    implicit val hnilType: Aux[HNil.type, HNil] = new NormalizeHList[HNil.type] {
      type Out = HNil
      def apply(l: HNil.type): HNil = HNil
    }

    implicit val hnilTrait: Aux[HNil, HNil] = new NormalizeHList[HNil] {
      type Out = HNil
      def apply(l: HNil): HNil = HNil
    }

    implicit def hcons[H, T <: HList, TO <: HList](
        implicit
        tail: Aux[T, TO]
    ): Aux[H :: T, H :: TO] = new NormalizeHList[H :: T] {
      type Out = H :: TO
      def apply(l: H :: T): H :: TO = l.head :: tail(l.tail)
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
