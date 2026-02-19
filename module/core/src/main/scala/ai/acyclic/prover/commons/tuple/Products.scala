package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.compat.{*:, TupleX}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.HNil

object Products {

  trait Cartesian extends Schemata.Cartesian {

    val Eye: Eye
    val T0: Eye.type = Eye
    val Nil: Eye.type = Eye
  }

  /**
    * cartesian product with a unique identity element.
    *
    * technically this applies to any Cartesian product, but in some libraries, Identity type is not a singleton. e.g.
    * shapeless.HNil type is a supertype of shapeless.HNil.type, despite being sealed.
    *
    * this is very annoying, as many operations defined for HNil have no variance
    */
  trait Cartesian_UID extends Cartesian {

    val eye: Prod
    override type Eye = eye.type
    override val Eye: Eye = eye

  }

  /**
    * same as schema, but with data & constructors
    */
  trait Monoidal extends Cartesian with Schemata.Monoidal {

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

        override def runtimeSeq: Vector[Element[? <: VBound]] = Vector.empty
        override lazy val toString: String = EMPTY
      }

      trait ><:[
          L <: VBound,
          TAIL <: Prod
      ] extends SchemaMixin.><:[L, TAIL]
          with Prod {

        def element: Element[L]

        override type Data = Element[L] *: tail.Data
        final override lazy val data: Data = element *: TupleX._ops(tail.data)

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
    }

  }
}
