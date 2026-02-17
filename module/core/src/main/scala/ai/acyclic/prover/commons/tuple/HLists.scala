package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.tuple.backbone.InductiveBackbone
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList, HNil}

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

/**
  * just BTuples with no bound, delegating to shapeless/formless
  */
object HLists extends Products.Monoidal {
  // TODO: unmaintained for Scala 3, should move to formless or interop backport that supports *: operator

  import shapeless.::

  override type VBound = Any
  override type Element[V <: VBound] = V

  override type Prod = HList
  type Tuple = Prod

  override type Eye = HNil
  val Eye = HNil

  infix type ><:[+HEAD <: VBound, +TAIL <: Prod] = HEAD :: TAIL

  infix type *:[+HEAD <: VBound, +TAIL <: Prod] = HEAD ><: TAIL

  override def cons[HEAD <: VBound, TAIL <: HList](head: HEAD, tail: TAIL): HEAD ><: TAIL = {

    head :: tail
  }

  override def deCons[HEAD <: VBound, TAIL <: HList](cons: HEAD *: TAIL): (HEAD, TAIL) = {
    cons.head -> cons.tail
  }

  implicit class Ops[H <: Tuple](hh: H) {

    // https://stackoverflow.com/questions/66036106/can-shapeless-record-type-be-used-as-a-poly1-part-2
    trait GetV extends Hom.Poly {

      implicit def getter[S](
          implicit
          _selector: shapeless.ops.hlist.Selector[H, S]
      ): Impl[S, _selector.Out] = at[S] { _ =>
        _selector(hh)
      }
    }
    object GetV extends GetV

    trait GetField extends Hom.Poly {

      implicit def getter[S](
          implicit
          _selector: shapeless.ops.record.Selector[H, S]
      ): Impl[S, FieldType[S, _selector.Out]] = at[S] { _ =>
        _selector(hh).asInstanceOf[FieldType[S, _selector.Out]]
      }
    }
    object GetField extends GetField
  }

  trait ToFlatRepr_Imp0 extends Poly {

    implicit def forTuples[I <: Prod, LN <: HList, O](
        implicit
        hlistToFlat: Tupler.Aux[I, O]
    ): I |- O = at[I] { i =>
      hlistToFlat(i)
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
      val (head, _) = deCons(v)
      head
    }

  }

  /**
    * The inverse of [[ToFlatRepr]]
    */
  object FromFlatRepr extends Poly with FromFlatRepr_LowPriority {

    implicit val forUnit: Unit |- Eye = at[Unit](_ => Eye)

    implicit def forProduct[P <: Product, O <: HList](
        implicit
        gen: Generic.Aux[P, O]
    ): P |- O = at[P] { p =>
      gen.to(p)
    }

  }

  trait FromFlatRepr_LowPriority {
    poly: Poly =>

    implicit def forValue[V <: VBound]: Element[V] |- (V ><: Eye) = at[Element[V]] { v =>
      cons(v, Eye)
    }
  }
}
