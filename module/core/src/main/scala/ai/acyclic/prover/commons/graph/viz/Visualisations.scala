package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.local.Local

import scala.language.implicitConversions

trait Visualisations {

  type Graph_/\[V] <: Local.AnyGraph[V]

  trait Format {

    trait TextViz[N] extends _TextViz[N] {

      final val outer = Format.this
    }
  }

  trait _TextViz[N] extends HasOuter {

    val outer: Format

    val semilattice: Graph_/\[N]

//    override lazy val toString: String = "[MISSING]"
  }

  object _TextViz {

    implicit def unbox[N](v: _TextViz[N]): Graph_/\[N] = v.semilattice
  }

}
