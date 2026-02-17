package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.compat.TupleX
import ai.acyclic.prover.commons.tuple.{HLists, Products}
import ai.acyclic.prover.commons.util.Phantom
import shapeless.HNil

trait Schema extends Scaffold {
  self: Singleton =>

  import HLists.*
  import InductiveBackbone.*

  override type Element[V <: VBound] = Unit

  trait Prod extends Serializable with Phantom {

    type HList <: TupleX.Prod
  }

  trait Eye extends Prod {

    override type HList = TupleX.Eye
  }
//  protected case object Eye extends Eye {}

  trait ><:[
      L <: VBound,
      +TAIL <: Prod
  ] extends Prod {

    val tail: TAIL

    override type HList <: L *: tail.HList
  }
}
