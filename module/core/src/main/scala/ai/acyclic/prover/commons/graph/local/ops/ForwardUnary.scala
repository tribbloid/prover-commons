package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait ForwardUnary extends Local.Forward.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Forward._Axiom]
  }

  def diagram_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[ArgV] = group.Viz(arg)
}

object ForwardUnary {

  case class ^[L <: Local.Forward._Axiom, V](
      argPlan: LocalEngine.PlanK.Compat[L, V],
      override val maxDepth: Int = 20
  ) extends ForwardUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
