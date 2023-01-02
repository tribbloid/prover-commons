package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.GraphSystem

import scala.language.implicitConversions

trait Visualisations {

  val graphSys: GraphSystem

  trait Format {

    lazy val printNodeFn: graphSys.Node => String = { v =>
      v.nodeText
    }
  }

  trait TextViz[N <: graphSys.Node] extends HasOuter {

    val outer: Format

    def node: N

    lazy val nodeString: String = outer.printNodeFn(node)

    def treeString: String
  }

  object TextViz {

    implicit def unbox[N <: graphSys.Node](v: TextViz[N]): N = v.node
  }

}
