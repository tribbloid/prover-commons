package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.local.Local

trait GraphBinary extends Local.Graph.Ops.Binary {

  override type Prev <: GraphUnary

  case class Union[VV]()(
      implicit
      ev1: prev.ArgV <:< VV,
      ev2: ArgV <:< VV
  ) extends Local.Graph.PlanEx[VV] {

    override def compute: Local.Graph[VV] = {

      val e1: Seq[prev.ArgNode] = prev.distinctEntries
      val e2: Seq[ArgNode] = arg.distinctEntries

      val roots1: Seq[prev.Arg.Node[VV]] = e1.map(n => n.upcast[VV])
      val roots2: Seq[Arg.Node[VV]] = e2.map(n => n.upcast[VV])

      Local.Graph.apply(
        (roots1 ++ roots2).distinct: _*
      )
    }
  }
}
