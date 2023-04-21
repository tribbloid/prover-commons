package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphKind
import shapeless.{Nat, Sized}

trait Arity {

  type N <: Nat

  trait Expressions[IG <: GraphKind.Like] {

    def args: Sized[Seq[Expression[IG]], N]

    trait To[OG <: GraphKind.Like] extends Expression[OG]
  }
}

object Arity {

  object Unary extends Arity {

    type N = Nat._1
  }

  object Binary extends Arity {

    type N = Nat._2
  }
}
