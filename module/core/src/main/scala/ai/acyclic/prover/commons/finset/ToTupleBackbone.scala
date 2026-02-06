package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons
import ai.acyclic.prover.commons.finset
import ai.acyclic.prover.commons.finset.ToTupleBackbone.EMPTY
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait ToTupleBackbone extends Finsets {

  import ToTupleBackbone.*

  trait Fin extends Product with Serializable {

    type Tuple <: Tuples.Fin
    val asTuple: Tuples.Fin
    lazy val asTupleOps: Tuples.InterOps[asTuple.type] = Tuples.InterOps(asTuple)

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
    override val asTuple: HNil = HNil
    override def asList: List[VBound] = Nil
    override lazy val toString: String = EMPTY
  }

  sealed trait ><[
      +TAIL <: Fin,
      +HEAD <: VBound
  ] extends Fin {

    val tail: TAIL
    val head: HEAD

    override lazy val asTuple = head :: tail.asTuple

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
}

object ToTupleBackbone {

  final val EMPTY = "∅"

  final val >< = " >< "
}
