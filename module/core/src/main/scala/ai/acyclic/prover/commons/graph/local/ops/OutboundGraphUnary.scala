package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait OutboundGraphUnary extends Local.AnyGraph.Outbound.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.AnyGraph.Outbound._Axiom]
  }

  def diagram_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[ArgV] = group.Viz(arg)
}

object OutboundGraphUnary {

  case class ^[L <: Local.AnyGraph.Outbound._Axiom, V](
      argPlan: LocalEngine.PlanK.Compat[L, V],
      override val maxDepth: Int = 20
  ) extends OutboundGraphUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
