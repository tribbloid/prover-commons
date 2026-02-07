package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.jit.hom.Hom.Poly

import scala.language.implicitConversions

/**
  * formerly "ProductTuples", this was mistake
  */
trait ToTupleMixin {
  self: Finsets =>
  // TODO: don't know how to implement it efficiently yet

  object Backbone extends ToTupleBackbone {
    override type VBound = self.VBound
  }

}

object ToTupleMixin {}
