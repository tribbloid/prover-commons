package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphK
import shapeless.{Nat, Sized}

trait Arity {

  type N <: Nat

  trait Expressions[IG <: GraphK.Like] {

    def args: Sized[Seq[Expression[IG]], N]

    trait To[OG <: GraphK.Like] extends Expression[OG]
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
