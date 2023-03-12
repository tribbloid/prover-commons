package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.GraphSystem

import scala.language.implicitConversions

trait Visualisations {

  type UB[N] <: GraphSystem.GraphK[N]

  trait Format {

    trait TextViz[N] extends _TextViz[N] {

      final val outer = Format.this
    }
  }

  trait _TextViz[N] extends HasOuter {

    val outer: Format

    val graph: UB[N]

    lazy val treeString: String = "[MISSING]"
  }

  object _TextViz {

    implicit def unbox[N](v: _TextViz[N]): UB[N] = v.graph
  }

}
