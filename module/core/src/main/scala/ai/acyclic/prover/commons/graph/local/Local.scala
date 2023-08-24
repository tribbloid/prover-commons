package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Arrow

object Local extends LocalEngine.Syntax {

  private def compileTimeCheck[V](): Unit = {

    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]

    implicitly[Tree[V] <:< Semilattice.Upper[V]]

    implicitly[Local.Tree.topology.LawImpl._Arrow <:< Arrow.`~>`.^]

    implicitly[Local.AnyGraph.Outbound.topology.LawImpl._Arrow <:< Local.Tree.topology.LawImpl._Arrow]

    val example = Local.AnyGraph.Outbound.empty[Int]
    implicitly[example.law._Arrow <:< Local.Tree.topology.LawImpl._Arrow]
  }
}
