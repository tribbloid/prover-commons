package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.tuple.HLists.*:
import ai.acyclic.prover.commons.tuple.HLists
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.HNil

trait InductiveBackbone extends Backbone {
  self: Singleton =>

  import InductiveBackbone.*

  object Schema extends SchemaBackbone {

    override type VBound = InductiveBackbone.this.VBound

  }

  trait Prod extends Schema.Prod with Serializable {

    val HList: HList
    final lazy val tupleOps: HLists.Ops[HList.type] = HLists.Ops(HList)

    def asList: List[Element[VBound]]
  }

  trait NativeTupleView[T] {

    val value: T
  }

  protected case object _1 extends Schema._1 with Prod {
    override val HList: HNil.type = HNil
    override def asList: List[Element[VBound]] = List.empty
    override lazy val toString: String = EMPTY
  }

  case class ><:[
      L <: VBound,
      +TAIL <: Prod
  ](
      val head: Element[L],
      override val tail: TAIL
  ) extends Schema.><:[L, TAIL](tail)
      with Prod {

    override def asList: List[Element[VBound]] = head.asInstanceOf[Element[VBound]] :: tail.asList

    override lazy val toString: String = {
      val tailStr =
        if (tail == _1) ""
        else " ><: " + tail.toString

      s"""${TextBlock(head.toString).indent("  ").build}$tailStr
         | """.stripMargin.trim
    }

    override type HList = L *: tail.HList
    override lazy val HList = head.asInstanceOf[L] :: tail.HList
  }

  final override def cons[HEAD <: VBound, TAIL <: Prod](head: Element[HEAD], tail: TAIL) =
    new ><:(head, tail)

  final override def deCons[HEAD <: VBound, TAIL <: Prod](
      cons: HEAD ><: TAIL
  ): (Element[HEAD], TAIL) = {

    cons match {
      case cons: ><:[head, tail] => (cons.head, cons.tail)
    }
  }

}

object InductiveBackbone {

  final val EMPTY = "∅"

  final val >< = " ><: "
}
