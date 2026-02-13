package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.{backbone, HLists, MonoidalProds}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait RecursiveHeapBackbone extends Backbone {
  self: Singleton =>

  import RecursiveHeapBackbone.*

  object Schema extends SchemaBackbone {

    override type VBound = RecursiveHeapBackbone.this.VBound

  }

  override type Element[V <: VBound] = V

  trait Prod extends Schema.Prod with Product with Serializable {

    val HList: HList
    final lazy val tupleOps: HLists.Ops[HList.type] = HLists.Ops(HList)

    def asList: List[VBound]
  }

  trait NativeTupleView[T] {

    val value: T
  }

  protected case object _1 extends Schema._1 with Prod {
    override val HList: HNil = HNil
    override def asList: List[VBound] = List.empty
    override lazy val toString: String = EMPTY
  }

  sealed trait ><:[
      +HEAD <: VBound,
      +TAIL <: Prod
  ] extends Schema.><:[HEAD, TAIL]
      with Prod {

    val head: HEAD
    val tail: TAIL

    override lazy val HList = head :: tail.HList

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

    override type HList = HEAD :: tail.HList

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
