package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.compat.{*:, TupleX}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.HList.ListCompat.::
import shapeless.HNil

abstract class Elements extends Backbone {

  import Elements.*

  object schema extends Schema { type VBound = Elements.this.VBound }

  trait Prod extends schema.Prod with Serializable {

    type Data <: TupleX.Prod
    val data: Data
    final lazy val hListOps = TupleX._ops(data)

    def runtimeList: List[Element[? <: VBound]]
  }

  trait Eye extends Prod with schema.Eye {

    type Data = HNil
    override val data: HNil.type = HNil
    override def runtimeList: List[Element[? <: VBound]] = List.empty
    override lazy val toString: String = EMPTY
  }

  trait ><:[
      L <: VBound,
      +TAIL <: Prod
  ] extends schema.><:[L, TAIL]
      with Prod {

    def element: Element[L]

    override type Data = Element[L] *: tail.Data
    override lazy val data: Data = element *: TupleX._ops(tail.data)

    override def runtimeList: List[Element[? <: VBound]] = tail.runtimeList.prepended(element)

    override lazy val toString: String = {
      val tailStr = tail match {
        case _: Eye => ""
        case _      => " ><: " + tail.toString
      }

      s"""${TextBlock(element.toString).indent("  ").build}$tailStr
         | """.stripMargin.trim
    }

  }
}

object Elements {

  final val EMPTY = "∅"

  final val >< = " ><: "
}
