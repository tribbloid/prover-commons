package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.compat.{*:, TupleX}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.HNil

object Products {

  /**
    * same as schema, but with data & constructors
    */
  trait Monoidal extends Schemata.Monoidal {

    def cons[L <: VBound, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL
    def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL)

    sealed trait _TupleOps[SELF <: Prod] {

      def self: SELF

      def ><:[
          L <: VBound
      ](
          head: Element[L]
      ): L ><: SELF = cons(head, self)
    }

    implicit class tupleOps[SELF <: Prod](val self: SELF) extends _TupleOps[SELF] {}

    protected object ElementsMixin extends Backbone {

      override type Element[V <: VBound] = Monoidal.this.Element[V]

      trait Prod extends SchemaMixin.Prod with Serializable {

        type Data <: TupleX.Prod
        val data: Data
        final lazy val dataOps = TupleX._ops(data)

        def runtimeSeq: Seq[Element[? <: VBound]]
      }

      trait Eye extends Prod with SchemaMixin.Eye {

        type Data = HNil
        override val data: HNil.type = HNil

        override def runtimeSeq: Seq[Element[? <: VBound]] = Seq.empty
        override lazy val toString: String = EMPTY
      }

      trait ><:[
          L <: VBound,
          TAIL <: Prod
      ] extends SchemaMixin.><:[L, TAIL]
          with Prod {

        def element: Element[L]

        override type Data = Element[L] *: tail.Data
        override lazy val data: Data = element *: TupleX._ops(tail.data)

        override def runtimeSeq: Seq[Element[? <: VBound]] = tail.runtimeSeq.prepended(element)

        override lazy val toString: String = {
          val tailStr = tail match {
            case _: Eye => ""
            case _      => " ><: " + tail.toString
          }

          s"""${TextBlock(element.toString).indent("  ").build}$tailStr
             | """.stripMargin.trim
        }
      }

      final val EMPTY = "∅"

      final val >< = " ><: "
    }
  }
}
