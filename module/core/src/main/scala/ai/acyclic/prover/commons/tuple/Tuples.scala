package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import shapeless.labelled.FieldType
//import shapeless.ops.record.Selector
import shapeless.{HList, HNil}

/**
  * just BTuples with no bound, delegating to shapeless/formless
  */
object Tuples extends MonoidalProds {
  // TODO: unmaintained for Scala 3, should move to formless or interop backport that supports *: operator

  import shapeless.::

  override type VBound = Any

  override type Element[V <: VBound] = V

  override type Prod = HList
  type Tuple = Prod

  override val _1: HNil.type = HNil

  type Unit = Eye
  val Unit: Nil = Eye

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
}
