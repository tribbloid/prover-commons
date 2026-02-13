package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait FlatReprMixin {
  self: MonoidalProds =>

  import self.*

  trait ToFlatRepr_Imp0 extends Poly {

    implicit def forTuples[I <: self.Prod, L <: HList, LN <: HList, O](
        implicit
        toHList: ToHList.|-[I, L],
        hlistToFlat: Tupler.Aux[L, O]
    ): I |- O = at[I] { i =>
      hlistToFlat(toHList(i))
    }
  }

  /**
    * can convert a [[Prod]] to a flat Scala tuple or Unit or value and back
    *
    * e.g.
    *   - (A, B) <-> A ><: B ><: Empty
    *   - (A) <-> A ><: Empty
    *   - A -> A ><: Empty
    *   - Unit -> Empty
    */
  object ToFlatRepr extends ToFlatRepr_Imp0 {

    implicit lazy val forUnit: Eye |- Unit = at[Eye](_ => ())

    implicit def forValue[V <: VBound]: (V ><: Eye) |- Element[V] = at[V ><: Eye] { v =>
      val (head, _) = self.deCons(v)
      head
    }

  }

  /**
    * The inverse of [[ToFlatRepr]]
    */
  object FromFlatRepr extends Poly with FromFlatRepr_LowPriority {

    implicit val forUnit: Unit |- Eye = at[Unit](_ => self.Eye)

    implicit def forProduct[P <: Product, L <: HList, O](
        implicit
        gen: Generic.Aux[P, L],
        fromTuple: FromHList.|-[L, O]
    ): P |- O = at[P] { p =>
      fromTuple(gen.to(p))
    }

  }

  trait FromFlatRepr_LowPriority {
    poly: Poly =>

    implicit def forValue[V <: VBound]: Element[V] |- (V ><: Eye) = at[Element[V]] { v =>
      self.cons(v, self.Eye)
    }
  }

}

object FlatReprMixin {}
