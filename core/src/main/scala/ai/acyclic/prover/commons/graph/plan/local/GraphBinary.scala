package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.Topology.GraphT
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.{Arity, Expression}
import shapeless.Sized

case class GraphBinary[IG <: Graph[N], N](arg1: Expression[IG], arg2: Expression[IG])
    extends Arity.Binary.Expressions[IG] {

  override lazy val args = Sized(arg1, arg2)

  lazy val inputGraph1 = arg1.exeOnce
  lazy val inputGraph2 = arg2.exeOnce

  object Union extends To[Graph[N]] {

    object ResultGraph extends Graph[N] {

      override def roots: Dataset[N] = inputGraph1.roots ++ inputGraph2.roots

      override protected def Ops: Node => GraphT.Ops[N] = ???
    }

    override def exe: Graph[N] = ???
  }
}
