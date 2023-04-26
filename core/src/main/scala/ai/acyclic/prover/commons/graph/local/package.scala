package ai.acyclic.prover.commons.graph

package object local extends Local.Syntax {

  private def compileTimeCheck[V](): Unit = {

    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]

    implicitly[Tree[V] <:< Semilattice.Upper[V]]

  }

}
