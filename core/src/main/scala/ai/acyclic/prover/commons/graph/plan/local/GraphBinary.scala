package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.GraphSystem._Graph
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.PlanExpr

case class GraphBinary[IG1 <: _Graph, IG2 <: _Graph, N](arg1: PlanExpr[IG1], arg2: PlanExpr[IG2])(
    implicit
    ev1: IG1 <:< Graph[N],
    ev2: IG2 <:< Graph[N]
) {

  val inputGraph1 = arg1.exeOnce
  val inputGraph2 = arg2.exeOnce

//  case object Union extends

}
