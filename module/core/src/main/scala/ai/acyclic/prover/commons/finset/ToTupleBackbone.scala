package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons
import ai.acyclic.prover.commons.finset
import ai.acyclic.prover.commons.finset.ToTupleBackbone.EMPTY
import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait ToTupleBackbone extends Finsets {

  import ToTupleBackbone.*

  trait Fin extends Product with Serializable {

    type Tuple <: Tuples.Fin
    val tuple: Tuples.Fin
    final lazy val tupleOps: Tuples.Ops[tuple.type] = Tuples.Ops(tuple)

    def asList: List[VBound]

    /**
      * return a Scala tuple with the same arity, e.g.
      *   - if Tuple is A :: B :: HNil, NativeTuple should be (A, B)
      *   - if Tuple is HNil, NativeTuple should be Unit
      */
//    def asNativeTuple[T](
//        implicit
//        v: Unit = ???
//    ): NativeTupleView[T] = {
//
//      // TODO: implement
//    }
  }

  trait NativeTupleView[T] {

    val value: T
  }

  protected case object _Empty extends Fin {
    override type Tuple = HNil
    override val tuple: HNil = HNil
    override def asList: List[VBound] = Nil
    override lazy val toString: String = EMPTY
  }

  sealed trait ><[
      +TAIL <: Fin,
      +HEAD <: VBound
  ] extends Fin {

    val tail: TAIL
    val head: HEAD

    override lazy val tuple = head :: tail.tuple

    override def asList: List[VBound] = tail.asList ++ Seq(head)

    override lazy val toString: String = {
      val tailStr =
        if (tail == _Empty) ""
        else tail.toString + " ><\n"

      s"""$tailStr${TextBlock(head.toString).indent("  ").build}
         | """.stripMargin.trim
    }
  }

  // cartesian product symbol
  case class Cons[
      TAIL <: Fin,
      HEAD <: VBound
  ](
      tail: TAIL,
      head: HEAD
  ) extends (TAIL >< HEAD) {

    // in scala 3 these will be gone
    type Tail = TAIL
    type Head = HEAD

    override type Tuple = HEAD :: tail.Tuple

  }

  final override def cons[TAIL <: Fin, HEAD <: VBound](tail: TAIL, head: HEAD) =
    Cons(tail, head)

  final override def deCons[TAIL <: Fin, HEAD <: VBound](
      cons: TAIL >< HEAD
  ): (TAIL, HEAD) = {

    cons match {
      case cons: Cons[tail, head] => (cons.tail, cons.head)
    }
  }

  trait FinToHList[-F <: Fin] {
    type Out <: HList
    def apply(f: F): Out
  }
  object FinToHList {
    type Aux[-F <: Fin, O <: HList] = FinToHList[F] { type Out = O }

    implicit val empty: Aux[Empty, HNil] = new FinToHList[Empty] {
      type Out = HNil
      def apply(f: Empty): HNil = HNil
    }

    implicit def cons[TAIL <: Fin, HEAD <: VBound, TO <: HList](
        implicit
        tailT: Aux[TAIL, TO]
    ): Aux[TAIL >< HEAD, HEAD :: TO] = new FinToHList[TAIL >< HEAD] {
      type Out = HEAD :: TO
      def apply(f: TAIL >< HEAD): HEAD :: TO = f.head :: tailT(f.tail)
    }
  }

  /**
    * Polymorphic function from [[Fin]] to Scala Tuple or Unit
    *
    * e.g.
    *   - Empty >< A >< B -> (A, B)
    *   - Empty -> Unit
    */
  trait ToFlatTuple extends Poly {

    implicit def generic[F <: Fin, L <: HList, Out](
        implicit
        toHList: FinToHList.Aux[F, L],
        tupler: shapeless.ops.hlist.Tupler.Aux[L, Out]
    ): F |- Out = at[F] { v =>
      tupler(toHList(v))
    }
  }
  object ToFlatTuple extends ToFlatTuple {}

  /**
    * Same as [[ToFlatTuple]], but convert Empty ><
    *
    * e.g.
    *   - Empty >< A >< B -> (A, B)
    *   - Empty -> Unit
    */
  trait ToFlat extends ToFlatTuple {

    implicit def singleton[H <: VBound]: (Empty >< H) |- H = at[Empty >< H] { v =>
      v.head
    }
  }
  object ToFlat extends ToFlat {}
}

object ToTupleBackbone {

  final val EMPTY = "∅"

  final val >< = " >< "
}
