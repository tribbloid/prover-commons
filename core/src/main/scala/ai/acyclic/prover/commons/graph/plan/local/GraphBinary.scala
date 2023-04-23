package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.local.{Graph, Local}
import ai.acyclic.prover.commons.graph.plan.PlanArg

trait GraphBinary extends PlanArg.Binary {

  type Prev <: GraphUnary

  type V
  override type LastInputG <: Graph[V]

  case class Union[VV]()(
      implicit
      ev1: prev.V <:< VV,
      ev2: V <:< VV
  ) extends To[Graph[VV]] {

    override def compute: Graph[VV] = {

      val roots1: Local.Dataset[Graph.LesserNode[VV]] =
        prev.lastInputG.roots.map((n: Graph.LesserNode[prev.V]) => n.upcast[VV])
      val roots2: Local.Dataset[Graph.LesserNode[VV]] =
        lastInputG.roots.map((n: Graph.LesserNode[V]) => n.upcast[VV])

      Graph.apply(
        (roots1 ++ roots2): _*
      )
    }
  }
}
