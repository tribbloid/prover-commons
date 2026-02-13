package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.{HLists, MonoidalProds}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait SchemaBackbone extends Backbone {
  self: Singleton =>

  import HLists.*
  import NestedBackbone.*

  override type Element[V <: VBound] = Unit

  trait Prod extends Serializable {

    type HList <: HLists.Prod
  }

  trait _1 extends Prod {

    override type HList = HNil.type
    override lazy val toString: String = EMPTY
  }
  protected case object _1 extends _1 {}

  class ><:[
      L <: VBound,
      +TAIL <: Prod
  ](
      val tail: TAIL
  ) extends Prod {

    override type HList <: L *: tail.HList
  }

  override def cons[L <: VBound, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL = new ><:(tail)

  override def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL) = (HLists.Unit, cons.tail)
}
