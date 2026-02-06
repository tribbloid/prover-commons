package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons
import ai.acyclic.prover.commons.finset
import ai.acyclic.prover.commons.finset.ToTupleBackbone.EYE
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait ToTupleBackbone extends Finsets {

  import ToTupleBackbone.*

  trait Fin {

    type _Tuple <: Tuples.Fin
    def asTuple: _Tuple
    lazy val asTupleOps: Tuples.InterOps[_Tuple] = Tuples.InterOps(asTuple)

    def asList: List[VBound]
  }

  sealed class Eye extends Fin {

    override type _Tuple = HNil
    override def asTuple: HNil = HNil

    override def asList: List[VBound] = Nil

    override lazy val toString: String = EYE
  }
  override val Eye = new Eye

  sealed trait ><[
      +TAIL <: Fin,
      +HEAD <: VBound
  ] extends Fin {

    val tail: TAIL
    val head: HEAD
  }

  // cartesian product symbol
  class ConsImpl[
      TAIL <: Fin,
      HEAD <: VBound
  ](
      val tail: TAIL,
      val head: HEAD
  ) extends (TAIL >< HEAD) {

    // in scala 3 these will be gone
    type Tail = TAIL
    type Head = HEAD

    override type _Tuple = HEAD :: tail._Tuple
    override def asTuple: _Tuple = head :: tail.asTuple

    override def asList: List[VBound] = tail.asList ++ Seq(head)

    override lazy val toString: String = {
      val tailStr = tail match {
        case _: Eye => ""
        case _      => tail.toString + " ><\n"
      }

      s"""$tailStr${TextBlock(head.toString).indent("  ").build}
         | """.stripMargin.trim
    }

  }

  final override def cons[TAIL <: Fin, HEAD <: VBound](tail: TAIL, head: HEAD) =
    new ConsImpl(tail, head)

  final override def deCons[TAIL <: Fin, HEAD <: VBound](
      cons: TAIL >< HEAD
  ): (TAIL, HEAD) = {

    cons match {
      case cons: ConsImpl[tail, head] => (cons.tail, cons.head)
    }
  }
}

object ToTupleBackbone {

  final val EYE = "∅"

  object W {

    final val eye = "Eye"

    final val >< = " >< "
  }
}
