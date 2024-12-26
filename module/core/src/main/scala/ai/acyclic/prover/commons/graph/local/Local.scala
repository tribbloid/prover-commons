package ai.acyclic.prover.commons.graph.local

object Local extends LocalEngine.Module {

  private def __sanity[V](): Unit = {

    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]

    implicitly[Tree[V] <:< Semilattice.Upper[V]]

//    implicitly[Local.Tree._Axiom <:< Arrow.Outbound]

    implicitly[Local.Tree._Axiom <:< Local.Semilattice._Axiom]
    implicitly[Local.Tree._Axiom <:< Local.AnyGraph.Outbound._Axiom]

    val example: Local.Tree[Int] = Local.Tree.empty[Int]
    implicitly[example._Axiom <:< Local.Tree._Axiom]
  }
}
