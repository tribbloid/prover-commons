package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphKind

trait PlanArg {

  type LastInputG <: GraphKind.Top
  def last: Plan[LastInputG]
  lazy val lastInputG: LastInputG = last.graph

  trait To[OG <: GraphKind.Top] extends Plan[OG]

  type Prev
  val prev: Prev
}

object PlanArg {

  trait Unary extends PlanArg {

    type Prev = Unit
    val prev: Unit = {}
  }

  trait Binary extends PlanArg {

    type Prev <: Unary
  }

  trait Ternary extends PlanArg {

    type Prev <: Binary
  }
}
