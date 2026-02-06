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

    type _Tuple <: Tuples.Fin
    val asTuple: Tuples.Fin
    lazy val asTupleOps: Tuples.InterOps[asTuple.type] = Tuples.InterOps(asTuple)

//    type _NativeTuple <: Product TODO: need to impl this later

    def asList: List[VBound]
  }

  case object _Empty extends Fin {
    override type _Tuple = HNil
    override val asTuple: HNil = HNil
    override def asList: List[VBound] = Nil
    override lazy val toString: String = EMPTY
  }
  override type Empty = _Empty.type
  override val Empty: _Empty.type = _Empty

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
  // cartesian product symbol
  case class ConsImpl[
      TAIL <: Fin,
      HEAD <: VBound
  ](
      tail: TAIL,
      head: HEAD
  ) extends (TAIL >< HEAD) {

    // in scala 3 these will be gone
    type Tail = TAIL
    type Head = HEAD

    override type _Tuple = HEAD :: tail._Tuple
  }

  final override def cons[TAIL <: Fin, HEAD <: VBound](tail: TAIL, head: HEAD) =
    ConsImpl(tail, head)

  final override def deCons[TAIL <: Fin, HEAD <: VBound](
      cons: TAIL >< HEAD
  ): (TAIL, HEAD) = {

    cons match {
      case cons: ConsImpl[tail, head] => (cons.tail, cons.head)
    }
  }
}

object ToTupleBackbone {

  final val EMPTY = "∅"

  final val >< = " >< "
}
