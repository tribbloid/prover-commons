package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.*
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy

trait OutboundGraphUnary extends Local.AnyGraph.Outbound.Ops.Unary {

//  {
//    implicitly[ArgLaw <:< Local.AnyGraph.Outbound._Axiom]
//  }

  def text_linkedHierarchy(
      implicit
      group: LinkedHierarchy#Group
  ): group.Viz[arg.Value] = group.Viz(arg)
}

object OutboundGraphUnary {

  case class ^[A <: Local.AnyGraph.Outbound.Graph[?]](
      arg: A,
      override val maxDepth: Int = 20
  ) extends OutboundGraphUnary {

    override type Arg = A
  }
}
