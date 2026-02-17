package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.compat.{*:, TupleX}
import ai.acyclic.prover.commons.tuple
import ai.acyclic.prover.commons.tuple.{backbone, Products}
import ai.acyclic.prover.commons.util.Phantom
import shapeless.HNil

trait Schema extends Backbone {
  self: Singleton =>

  override type Element[V <: VBound] = Unit

  trait Prod extends Serializable with Phantom {

    type Header <: TupleX.Prod
  }

  trait Eye extends Prod {

    override type Header = TupleX.Eye
  }

  trait ><:[
      L <: VBound,
      +TAIL <: Prod
  ] extends Prod {

    val tail: TAIL

    override type Header = L *: tail.Header
  }
}
