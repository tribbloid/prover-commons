package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.tuple.HLists.*:
import ai.acyclic.prover.commons.tuple.HLists
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.HList.ListCompat.::
import shapeless.HNil

abstract class InductiveBackbone(val schema: Schema) extends Scaffold {

  import InductiveBackbone.*

  type VBound = schema.VBound

  override type Element[+T]

  trait Prod extends schema.Prod with Serializable {

    val hList: HList
    final lazy val hListOps: HLists.Ops[hList.type] = HLists.Ops(hList)

    def runtimeList: List[Element[VBound]]
  }

  trait Eye extends Prod with schema.Eye {

    override val hList: HNil.type = HNil
    override def runtimeList: List[Element[VBound]] = List.empty
    override lazy val toString: String = EMPTY
  }

  trait ><:[
      L <: VBound,
      +TAIL <: Prod
  ] extends schema.><:[L, TAIL]
      with Prod {

    def element: Element[L]

    override def runtimeList: List[Element[VBound]] = tail.runtimeList.prepended(element)

    override lazy val toString: String = {
      val tailStr = tail match {
        case _: Eye => ""
        case _      => " ><: " + tail.toString
      }

      s"""${TextBlock(element.toString).indent("  ").build}$tailStr
         | """.stripMargin.trim
    }

    override type HList = Element[L] *: tail.HList
    override lazy val hList: HList = element :: tail.hList
  }

  type *:[L <: VBound, +TAIL <: Prod] = ><:[L, TAIL]
}

object InductiveBackbone {

  final val EMPTY = "∅"

  final val >< = " ><: "
}
