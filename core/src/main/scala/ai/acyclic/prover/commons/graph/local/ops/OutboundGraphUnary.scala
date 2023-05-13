package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait OutboundGraphUnary extends Local.Graph.Outbound.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Graph.Outbound._L]
  }

  def diagram_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[ArgV] = group.Viz(arg)
}

object OutboundGraphUnary {

  case class ^[L <: Local.Graph.Outbound._L, V](argPlan: LocalEngine.PlanKind.Aux[L, V]) extends OutboundGraphUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}