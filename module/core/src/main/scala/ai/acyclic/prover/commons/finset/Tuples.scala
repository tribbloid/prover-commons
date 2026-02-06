package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.jit.hom.Hom
import shapeless.labelled.FieldType
//import shapeless.ops.record.Selector
import shapeless.{HList, HNil}

/**
  * just a Finsets with no bound, delegating to shapeless
  */
object Tuples extends Finsets {
  // TODO: unmaintained for Scala 3, should move to formless or interop backport that supports *: operator

  import shapeless.::

  override type VBound = Any
  override type Fin = HList

  type Tuple = Fin

  override type Empty = HNil
  override val Empty: HNil = HNil

  type Unit = Empty
  val Unit = Empty

  infix type ><[+TAIL <: Fin, +HEAD <: VBound] = HEAD :: TAIL

  type :*[+TAIL <: Fin, +HEAD <: VBound] = HEAD :: TAIL

  override def cons[TAIL <: HList, HEAD](tail: TAIL, head: HEAD): TAIL >< HEAD = {

    head :: tail
  }

  override def deCons[TAIL <: HList, HEAD <: VBound](cons: TAIL :* HEAD): (TAIL, HEAD) = {
    cons.tail -> cons.head
  }

  implicit class InterOps[H <: Fin](hh: H) {

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
}
