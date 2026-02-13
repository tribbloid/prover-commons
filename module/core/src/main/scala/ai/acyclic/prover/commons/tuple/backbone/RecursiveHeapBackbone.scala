package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.{MonoidalProds, Tuples}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait RecursiveHeapBackbone extends Backbone {
  self: Singleton =>

  import RecursiveHeapBackbone.*

  trait Prod extends Product with Serializable {

    type Tuple <: Tuples.Prod
    val tuple: Tuples.Prod
    final lazy val tupleOps: Tuples.Ops[tuple.type] = Tuples.Ops(tuple)

    def asList: List[VBound]
  }

  trait NativeTupleView[T] {

    val value: T
  }

  protected case object _1 extends Prod {
    override type Tuple = HNil.type
    override val tuple: HNil = HNil
    override def asList: List[VBound] = List.empty
    override lazy val toString: String = EMPTY
  }

  sealed trait ><:[
      +HEAD <: VBound,
      +TAIL <: Prod
  ] extends Prod {

    val head: HEAD
    val tail: TAIL

    override lazy val tuple = head :: tail.tuple

    override def asList: List[VBound] = head :: tail.asList

    override lazy val toString: String = {
      val tailStr =
        if (tail == _1) ""
        else " ><: " + tail.toString

      s"""${TextBlock(head.toString).indent("  ").build}$tailStr
         | """.stripMargin.trim
    }
  }

  // cartesian product symbol
  case class Cons[
      HEAD <: VBound,
      TAIL <: Prod
  ](
      head: HEAD,
      tail: TAIL
  ) extends (HEAD ><: TAIL) {

    // in scala 3 these will be gone
    type Head = HEAD
    type Tail = TAIL

    override type Tuple = HEAD :: tail.Tuple

  }

  final override def cons[HEAD <: VBound, TAIL <: Prod](head: HEAD, tail: TAIL) =
    Cons(head, tail)

  final override def deCons[HEAD <: VBound, TAIL <: Prod](
      cons: HEAD ><: TAIL
  ): (HEAD, TAIL) = {

    cons match {
      case cons: Cons[head, tail] => (cons.head, cons.tail)
    }
  }

}

object RecursiveHeapBackbone {

  final val EMPTY = "∅"

  final val >< = " ><: "
}
