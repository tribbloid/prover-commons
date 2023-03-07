package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphSystem
import shapeless.{nat, Nat, Sized}

trait PlanGroup {

  type Arity <: Nat

  trait Expressions[IG <: GraphSystem._Graph] {

    def args: Sized[Seq[GraphExpr[IG]], Arity]

    trait Expr[OG <: GraphSystem._Graph] extends GraphExpr[OG]
  }
}

object PlanGroup {

  object Unary extends PlanGroup {

    type Arity = Nat._1
  }

  object Binary extends PlanGroup {

    type Arity = Nat._2
  }
}
