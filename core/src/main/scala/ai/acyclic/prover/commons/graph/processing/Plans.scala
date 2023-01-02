package ai.acyclic.prover.commons.graph.processing

import ai.acyclic.prover.commons.graph.GraphSystem

trait Plans {

  val graphSys: GraphSystem

  trait Plan {}

  trait Unary extends Plan {

    def exeOn(node: graphSys.Node): Unit
  }

}

object Plans {
//  trait Unary extends PlanSystem {
//
////    type Unary <: ThisUnary
//
//    //  type UnaryAux[T] = Unary { type NodeUB = T }
//  }
}
