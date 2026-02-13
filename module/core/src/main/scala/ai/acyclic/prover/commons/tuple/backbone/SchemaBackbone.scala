package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import ai.acyclic.prover.commons.tuple.{HLists, MonoidalProds}
import ai.acyclic.prover.commons.typesetting.TextBlock
import shapeless.{::, HList, HNil}

import scala.language.implicitConversions

trait SchemaBackbone extends Backbone {
  self: Singleton =>

  import RecursiveHeapBackbone.*

  override type Element[V <: VBound] = Unit

  trait Prod extends Product with Serializable {

    type HList <: HLists.Prod
  }

  trait _1 extends Prod {

    override type HList = HNil.type
    override lazy val toString: String = EMPTY
  }
  protected case object _1 extends _1 {}

  trait ><:[
      +HEAD <: VBound,
      +TAIL <: Prod
  ] extends Prod {
    import HLists.*

    override type HList = HEAD *: TAIL
  }

}
