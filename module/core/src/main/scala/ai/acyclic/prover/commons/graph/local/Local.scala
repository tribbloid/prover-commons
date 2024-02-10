package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Arrow

object Local extends LocalEngine.Module {

  private def compileTimeCheck[V](): Unit = {

    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]

    implicitly[Tree[V] <:< Semilattice.Upper[V]]

    implicitly[Local.Tree._Arrow <:< Arrow.`~>`.^]

    implicitly[Local.AnyGraph.Outbound._Arrow <:< Local.Tree._Arrow]

    val example = Local.AnyGraph.Outbound.empty[Int]
    implicitly[example._Arrow <:< Local.Tree._Arrow]
  }
}
