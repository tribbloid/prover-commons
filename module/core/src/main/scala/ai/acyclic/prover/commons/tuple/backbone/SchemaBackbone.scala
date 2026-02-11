package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.{BTuples, Tuples}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait SchemaBackbone extends Backbone {
  self: Singleton =>

  import RecursiveHeapBackbone.*

  trait Prod extends Product with Serializable {}

  trait NativeTupleView[T] {

    val value: T
  }

  protected case object _1 extends Prod {
    override lazy val toString: String = EMPTY

  }

  sealed trait ><:[
      +HEAD <: VBound,
      +TAIL <: Prod
  ] extends Prod {}

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
