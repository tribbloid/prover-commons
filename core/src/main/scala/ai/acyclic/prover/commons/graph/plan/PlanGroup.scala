package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphK
import shapeless.{Nat, Sized}

trait PlanGroup {

  type Arity <: Nat

  trait Expressions[IG <: GraphK.Like] {

    def args: Sized[Seq[PlanExpr[IG]], Arity]

    trait Expr[OG <: GraphK.Like] extends PlanExpr[OG]
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
