//package ai.acyclic.prover.commons.graph.plan.local
//
//import ai.acyclic.prover.commons.graph.GraphSystem._Graph
//import ai.acyclic.prover.commons.graph.local.{Graph, Poset}
//import ai.acyclic.prover.commons.graph.plan.{PlanExpr, PlanGroup}
//import shapeless.Sized
//
//case class PosetUnary[IG <: _Graph, N](arg: PlanExpr[IG])(
//    implicit
//    ev: IG <:< Poset[N]
//    // see GraphUnary counterpart
//) extends PlanGroup.Unary.Expressions[IG] {
//
//  final override lazy val args = Sized(arg)
//
//  lazy val inputGraph: IG = arg.exeOnce
//
//}
