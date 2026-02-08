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

  type _0 = HNil.type
  val _0: _0 = HNil

  override val _Empty: _0 = _0

  infix type ><:[+HEAD <: VBound, +TAIL <: Fin] = HEAD :: TAIL

  infix type :*[+HEAD <: VBound, +TAIL <: Fin] = HEAD ><: TAIL

  override def cons[HEAD, TAIL <: HList](head: HEAD, tail: TAIL): HEAD ><: TAIL = {

    head :: tail
  }

  override def deCons[HEAD <: VBound, TAIL <: HList](cons: HEAD :* TAIL): (HEAD, TAIL) = {
    cons.head -> cons.tail
  }

  implicit class Ops[H <: Fin](hh: H) {

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
