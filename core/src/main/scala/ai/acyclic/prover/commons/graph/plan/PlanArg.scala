package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph

trait PlanArg {

  type Input <: Plan
  val inputPlan: Input

  final val topology: inputPlan.OT = inputPlan.outTopology

  type IGK[v] = inputPlan.OGK[v]
  type IG = inputPlan.OG
  final val inputG: IG = inputPlan.resolve

  type IV = inputPlan.OV

  type IN = inputPlan.ON

  type Rewriter = topology.Rewriter[IV]

  abstract class To[V] extends Plan {

    type OT = inputPlan.OT
    val outTopology: inputPlan.OT = inputPlan.outTopology

    type OV = V
  }

  type Prev
  val prev: Prev
}

object PlanArg {

  type Aux[P <: Plan] = PlanArg { type Input = P }
  trait AuxEx[P <: Plan] extends PlanArg { type Input = P }

  trait Unary extends PlanArg {

    type Prev = Unit
    val prev: Unit = {}
  }

  trait Binary extends PlanArg {

    type Prev <: Unary
  }

  private def compileTimeCheck(): Unit = {

    object Dummy extends AuxEx[graph.local.Graph]

    case class Dummy[P <: Plan](override val inputPlan: P) extends AuxEx[P] with Unary {}

    val entries = Dummy(Graph.apply())
    implicitly[entries.type <:< Vector[IN]]
  }

}
