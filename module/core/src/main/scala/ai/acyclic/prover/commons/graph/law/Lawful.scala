package ai.acyclic.prover.commons.graph.law

import ai.acyclic.prover.commons.graph.law.Topology.Law
import ai.acyclic.prover.commons.graph.{Arrow, NodeK, RewriterK}

trait Lawful {
  type _Law <: Law

  type _Arrow <: Arrow

  type Node[v] = NodeK.Compat[_Law, v]

  type Rewriter[v] = RewriterK.Aux[_Law, v]
}

object Lawful {

  trait Construct[+L <: Law] {

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
