package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait OutboundGraphUnary extends Local.AnyGraph.Outbound.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.AnyGraph.Outbound.Law_/\]
  }

  def diagram_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[ArgV] = group.Viz(arg)
}

object OutboundGraphUnary {

  case class ^[L <: Local.AnyGraph.Outbound.Law_/\, V](argPlan: LocalEngine.PlanK.Aux[L, V]) extends OutboundGraphUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
