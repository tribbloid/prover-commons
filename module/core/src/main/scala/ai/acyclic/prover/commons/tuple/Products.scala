package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.compat.{*:, TupleX}
import ai.acyclic.prover.commons.jit.hom.Hom.Poly

import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{Generic, HNil}

object Products {

  trait Cartesian extends Schemata.Cartesian {

    val Eye: Eye
    final lazy val T0: Eye.type = Eye
    final lazy val Nil: Eye.type = Eye
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
    override lazy val Eye: Eye = eye

  }

  /**
    * same as schema, but with data & constructors
    */
  trait Monoidal extends Cartesian with Schemata.Monoidal {

    protected def cons[L <: VBound, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL
    def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL)

    trait VarArgsConstructor {

      def applyProduct[L <: Prod](list: L): L = list
    }

    object of extends VarArgsConstructor with shapeless.ProductArgs {}

    object ofNarrow extends VarArgsConstructor with shapeless.SingletonProductArgs {}

    /**
      * The inverse of [[Ops.ToFlatTuple]]
      */
    object FromProductOrValue extends FromProductOrValue_Imp0 {

      implicit val _unit: Unit /=> Eye = at[Unit](_ => Eye)

      implicit def _product[P <: Product, O <: Prod](
          implicit
          gen: Generic.Aux[P, O]
      ): P /=> O = at[P] { p =>
        gen.to(p)
      }
    }

    protected trait FromProductOrValue_Imp0 extends Poly {

      implicit def _value[V <: VBound]: Element[V] /=> (V ><: Eye) = at[Element[V]] { v =>
        cons(v, Eye)
      }
    }

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

        def head: Element[L]

        override type Data = Element[L] *: tail.Data
        final override lazy val data: Data = head *: TupleX._ops(tail.data)

        override lazy val toString: String = {
          val tailStr = tail match {
            case _: Eye => ""
            case _      => " ><: " + tail.toString
          }

          s"""${TextBlock(head.toString).indent("  ").build}$tailStr
             | """.stripMargin.trim
        }
      }

      final val EMPTY = "∅"
    }

    /**
      * type class to zip 2 products together
      *
      * e.g. zip((A ><: B ><: Eye), (C ><: D ><: Eye)) = A ><: B ><: C ><: D ><: Eye
      *
      * unzip is the inverse operation
      */
    trait Zippable[A <: Prod, B <: Prod, Y <: Prod] {

      type Zipped = Y

      def zip(a: A, b: B): Y

      def unzip(ab: Y): (A, B)
    }

    object Zippable {

      type Zip[-A <: Prod, -B <: Prod] = Zippable[? >: A, ? >: B, ?]
      type Unzip[-Y <: Prod] = Zippable[?, ?, ? >: Y]

      type Aux[A <: Prod, B <: Prod, O <: Prod] = Zippable[A, B, O] { type Zipped = O }
//
      implicit def empty[B <: Prod]: Zippable[Eye, B, B] = new Zippable[Eye, B, B] {
        override type Zipped = B

        def zip(a: Eye, b: B): B = b

        def unzip(ab: B): (Eye, B) = (Eye, ab)
      }

      implicit def cons[HEAD <: VBound, TAIL <: Prod, B <: Prod, O <: Prod](
          implicit
          tailZip: Aux[TAIL, B, O]
      ): Aux[HEAD ><: TAIL, B, HEAD ><: O] = new Zippable[HEAD ><: TAIL, B, HEAD ><: O] {
        override type Zipped = HEAD ><: O

        def zip(a: HEAD ><: TAIL, b: B): HEAD ><: O = {
          val (head, tail) = deCons(a)
          head ><: tailZip.zip(tail, b)
        }

        def unzip(ab: HEAD ><: O): (HEAD ><: TAIL, B) = {
          val (head, o) = deCons(ab)
          val (tail, b) = tailZip.unzip(o)
          (head ><: tail, b)
        }
      }
    }
  }

}
