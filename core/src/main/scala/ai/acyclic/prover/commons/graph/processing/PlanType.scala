package ai.acyclic.prover.commons.graph.processing

import ai.acyclic.prover.commons.graph.GraphSystem

trait PlanType {

  // cannot cross systems

  type OUB[N] <: GraphSystem.GraphK[N]

  trait Plan[N] {

    def exe: OUB[N]

    final lazy val exeOnce: OUB[N] = exe
  }

  trait Unary[N] extends Plan[N] {}
}

object PlanType {}
