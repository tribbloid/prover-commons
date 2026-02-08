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

  sealed trait ><:[
      +HEAD <: VBound,
      +TAIL <: Fin
  ] extends Fin {

    val head: HEAD
    val tail: TAIL

    override lazy val tuple = head :: tail.tuple

    override def asList: List[VBound] = head :: tail.asList

    override lazy val toString: String = {
      val tailStr =
        if (tail == _Empty) ""
        else " ><: " + tail.toString

      s"""${TextBlock(head.toString).indent("  ").build}$tailStr
         | """.stripMargin.trim
    }
  }

  // cartesian product symbol
  case class Cons[
      HEAD <: VBound,
      TAIL <: Fin
  ](
      head: HEAD,
      tail: TAIL
  ) extends (HEAD ><: TAIL) {

    // in scala 3 these will be gone
    type Head = HEAD
    type Tail = TAIL

    override type Tuple = HEAD :: tail.Tuple

  }

  final override def cons[HEAD <: VBound, TAIL <: Fin](head: HEAD, tail: TAIL) =
    Cons(head, tail)

  final override def deCons[HEAD <: VBound, TAIL <: Fin](
      cons: HEAD ><: TAIL
  ): (HEAD, TAIL) = {

    cons match {
      case cons: Cons[head, tail] => (cons.head, cons.tail)
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

    implicit def cons[HEAD <: VBound, TAIL <: Fin, TO <: HList](
        implicit
        tailT: Aux[TAIL, TO]
    ): Aux[HEAD ><: TAIL, HEAD :: TO] = new FinToHList[HEAD ><: TAIL] {
      type Out = HEAD :: TO
      def apply(f: HEAD ><: TAIL): HEAD :: TO = f.head :: tailT(f.tail)
    }
  }

  /**
    * Polymorphic function from [[Fin]] to Scala Tuple or Unit
    *
    * e.g.
    *   - A ><: B ><: Empty -> (A, B)
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
    *   - A ><: B ><: Empty -> (A, B)
    *   - Empty -> Unit
    */
  trait ToFlat extends ToFlatTuple {

    implicit def singleton[H <: VBound]: (H ><: Empty) |- H = at[H ><: Empty] { v =>
      v.head
    }
  }
  object ToFlat extends ToFlat {}
}

object ToTupleBackbone {

  final val EMPTY = "∅"

  final val >< = " ><: "
}
