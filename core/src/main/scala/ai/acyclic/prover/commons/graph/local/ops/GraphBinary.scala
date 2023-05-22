package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.Local

trait GraphBinary extends Local.AnyGraph.Ops.Binary {

  override type Prev <: AnyGraphUnary

  case class Union[VV]()(
      implicit
      ev1: prev.ArgV <:< VV,
      ev2: ArgV <:< VV
  ) extends Local.AnyGraph.PlanEx[VV] {

    override def compute: Local.AnyGraph[VV] = {

      val e1: Seq[prev.ArgNode] = prev.distinctEntries
      val e2: Seq[ArgNode] = arg.distinctEntries

      val roots1: Seq[prev.Arg.Node[VV]] = e1.map(n => n.upcast[VV])
      val roots2: Seq[Arg.Node[VV]] = e2.map(n => n.upcast[VV])

      Local.AnyGraph.make(
        (roots1 ++ roots2).distinct: _*
      )
    }
  }
}

object GraphBinary {}
