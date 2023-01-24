package ai.acyclic.prover.commons.graph.processing

import ai.acyclic.prover.commons.graph.GraphSystem

trait PlanType {

  // cannot cross systems

  type OUB[N] <: GraphSystem.GraphK[N]

  trait Plan[N] {

    def exe: OUB[N]

  }

  trait Unary[N] extends Plan[N] {}
}

object PlanType {}
