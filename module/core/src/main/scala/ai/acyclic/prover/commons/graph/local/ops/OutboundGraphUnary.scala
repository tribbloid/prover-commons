package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait OutboundGraphUnary extends Local.AnyGraph.Outbound.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.AnyGraph.Outbound._Law]
  }

  def diagram_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[ArgV] = group.Viz(arg)
}

object OutboundGraphUnary {

  case class ^[L <: Local.AnyGraph.Outbound._Law, V](
      argPlan: LocalEngine.PlanK.Aux[L, V],
      override val maxDepth: Int = 20
  ) extends OutboundGraphUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
