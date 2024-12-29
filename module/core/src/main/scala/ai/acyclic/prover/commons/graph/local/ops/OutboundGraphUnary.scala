package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait OutboundGraphUnary extends Local.AnyGraph.Outbound.Ops.Unary {

  {
    implicitly[arg.axiom.type <:< Local.AnyGraph.Outbound._Axiom]
  }

  def text_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[arg.Value] = group.Viz(arg)
}

object OutboundGraphUnary {

  case class ^[A <: Local.AnyGraph.Outbound[?]](
      override val arg: A,
      override val maxDepth: Int = 20
  ) extends OutboundGraphUnary {

    override type Arg = A
  }
}
