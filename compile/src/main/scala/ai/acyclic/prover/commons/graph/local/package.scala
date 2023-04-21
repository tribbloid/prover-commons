package ai.acyclic.prover.commons.graph

package object local extends Local.Aliases {

  private def compileTimeSanityCheck[V](): Unit = {

    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]

    implicitly[Tree[V] <:< Semilattice.Upper[V]]

  }
}
