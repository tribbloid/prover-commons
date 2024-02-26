package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

trait ProductTuples[VB] extends Tuples {

  import ProductTuples.*

  final type VBound = VB

  trait Tuple {

    type Static <: HList
    def static: Static
    lazy val staticView: HListView[Static] = HListView(static)

    def asList: List[VB]
  }

  class Eye extends Tuple {

    override type Static = HNil
    override def static: HNil = HNil

    override def asList: List[VB] = Nil

    override lazy val toString: String = EYE
  }
  override val Eye = new Eye

  // cartesian product symbol
  class ><[
      TAIL <: Tuple,
      HEAD <: VB
  ](
      val tail: TAIL,
      val head: HEAD
  ) extends Tuple {

    // in scala 3 these will be gone
    type Tail = TAIL
    type Head = HEAD

    override type Static = HEAD :: tail.Static
    override def static: Static = head :: tail.static

    override def asList: List[VB] = tail.asList ++ Seq(head)

    override lazy val toString: String = {
      val tailStr = tail match {
        case _: Eye => ""
        case _      => tail.toString + " ><\n"
      }

      s"""$tailStr${TextBlock(head.toString).indent("  ").build}
         | """.stripMargin.trim
    }

  }

  final override def cons[TAIL <: Tuple, HEAD <: VBound](tail: TAIL, head: HEAD): TAIL >< HEAD =
    new ><(tail, head)
}

object ProductTuples {

  final val EYE = "∅"

  object W {

    final val eye = "Eye"

    final val >< = " >< "
  }
}
