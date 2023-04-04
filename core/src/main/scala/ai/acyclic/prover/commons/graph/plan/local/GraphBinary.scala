package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.GraphK
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.{Expression, PlanGroup}
import shapeless.Sized

case class GraphBinary[IG <: GraphK.Like, N](arg1: Expression[IG], arg2: Expression[IG])(
    implicit
    ev1: IG <:< Graph[N],
    ev2: IG <:< Graph[N]
) extends PlanGroup.Binary.Expressions[IG] {

  override lazy val args = Sized(arg1, arg2)

  lazy val inputGraph1 = arg1.exeOnce
  lazy val inputGraph2 = arg2.exeOnce

//  object Union extends Expr[Graph[N]] {
//
//    object ResultGraph extends Graph[N] {
//
//      case class Ops(node: N) extends GraphOps {
//
//        override protected def getInduction: Seq[Arrow.Of[Node]] = ???
//      }
//
//      override def roots: Dataset[N] = ???
//    }
//
//    override def exe: Graph[N] = ???
//  }
}
