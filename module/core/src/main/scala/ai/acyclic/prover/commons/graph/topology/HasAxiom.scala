package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.util.StaticGroup

//trait HasAxiomUniverse {
//
//  type AxiomUniverse[A] = AxiomUniverse.Aux[Axiom.Instance]
//
//  object AxiomUniverse extends StaticGroup {
//
//    trait Inst extends Case {
//
//      type Axiom
//
//      type _Arrow <: Arrow
//    }
//
//    type Aux[A <: Axiom] = Inst { type Axiom = A }
//  }
//
//
//
//}

trait HasAxiom {

  /**
    * a container of graph constraints
    */
  type Axiom = Axiom.Instance

  object Axiom extends StaticGroup {

    trait Instance extends Case {}

    type Top = Topology.AnyGraph._Axiom

  }

}
