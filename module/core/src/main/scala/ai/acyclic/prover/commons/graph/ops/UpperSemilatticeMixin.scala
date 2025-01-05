package ai.acyclic.prover.commons.graph.ops

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.topology.DivergingForm

trait UpperSemilatticeMixin {
  self: Engine & PosetMixin =>

  case class UpperSemilattice1[
      X <: DivergingForm.UpperSemilattice._Axiom,
      V
  ](
      override val arg: Graph[X, V]
  ) extends Ops.Unary[X, V](arg) {

    def asUpperSemilattice1Ops: this.type = this

    lazy val maxNodeOpt: Option[ArgNode] = {

      arg.maxNodes.headOption
    }
  }

  implicit def imp_upperSemilattice[
      X <: DivergingForm.UpperSemilattice._Axiom,
      V
  ](
      arg: Graph[X, V]
  ): UpperSemilattice1[X, V] = UpperSemilattice1[X, V](arg)
}
