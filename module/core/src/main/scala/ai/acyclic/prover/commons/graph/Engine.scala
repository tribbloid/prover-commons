package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, Topology}

trait Engine extends Priors.HasBatch {
  self: Singleton =>

  import Engine.*

  type Node[+X <: Axiom.Top, +V] = Foundation.NodeK.Lt[X, V]
  type Setter[X <: Axiom.Top, V] = Foundation.Setter.Aux[X, V]

  trait Graph[+X <: Axiom.Top] extends Foundation.GraphK[X] {

    final override lazy val engine: Engine.this.type = Engine.this
  }

  object Graph {

    type Aux[+X <: Axiom.Top, V] = Graph[X] { type Value = V }
    trait Aux_[+X <: Axiom.Top, V] extends Graph[X] { type Value = V }

    type Lt[+X <: Axiom.Top, +V] = Graph[X] { type Value <: V }

    /**
      * Graph representation without any validation
      */
    case class Unchecked[X <: Axiom.Top, V](
        entries: Batch[Foundation.NodeK.Lt[X, V]]
    )(
        override val axiom: X
    ) extends Graph[X] {

      type _Axiom = X

      type Value = V

      { // sanity

        implicitly[Unchecked[X, V] <:< Graph.Aux[X, V]]
        implicitly[Unchecked[X, V] <:< Graph.Lt[X, V]]

      }
      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  private def makeWithAxioms[XX <: Axiom.Top, V](
      nodes: Foundation.NodeK.Lt[XX, V]*
  )(
      assuming: XX
  ): Graph.Unchecked[XX, V] =
    Graph.Unchecked[XX, V](parallelize(nodes))(assuming)

  abstract class GraphType[X <: Axiom.Top](
      val axiom: X // this is a phantom object only used to infer type parameters
  ) extends Foundation.Lawful {

    type _Axiom = X
    type Graph[+V] = Graph.Lt[X, V]

    abstract class Plan[V] extends Graph.Aux_[X, V] {

      override val axiom: GraphType.this.axiom.type = axiom
    }

    def makeExact[V](
        nodes: Foundation.NodeK.Lt[X, V]*
    ): Graph.Unchecked[X, V] =
      makeWithAxioms[X, V](nodes*)(this.axiom)

    def empty[V]: Graph[V] = makeExact[V]()

    object makeTightest {

      def apply[XX <: X, V](
          nodes: Foundation.NodeK.Lt[XX, V]*
      )(
          implicit
          assuming: XX
      ): Graph.Unchecked[XX, V] =
        makeWithAxioms[XX, V](nodes*)(assuming)
    }
  }

  implicit def graphTypeAsMake(v: GraphType[?]): v.makeTightest.type = v.makeTightest

  sealed abstract class GraphImpls[X <: Axiom.Top, A <: Arrow](
      val topologyImpls: Topology.Impls[X, A] // this is a phantom object only used to infer type parameters
  ) extends GraphType[X](topologyImpls.concreteAxiom) {

    type Node_[V] = topologyImpls.Node_[V]
    type Setter_[V] = topologyImpls.Setter_[V]

    type NodeGroup = topologyImpls.NodeGroup

    type Inspection[V] = topologyImpls.Inspection[V]
  }

  abstract class Ops[
      X <: Axiom.Top,
      V
  ](
      val arg: Graph.Lt[X, V]
  ) extends HasMaxRecursionDepth {

    type Prev
    val prev: Prev

    type ArgNode = Foundation.NodeK.Lt[X, V]
    type ArgSetter = Foundation.Setter.Aux[X, V]

  }

  object Ops {

    trait Unary[
        X <: Axiom.Top,
        V
    ] extends Ops[X, V] {

      type Prev = Unit
      val prev: Unit = {}

      abstract class Plan[V] extends Graph.Aux_[X, V] {

        override val axiom: arg.axiom.type = arg.axiom
      }
    }

    trait Binary[
        X <: Axiom.Top,
        V
    ] extends Ops[X, V] {

      type Prev = Unary[X, V]
    }
  }

  object AnyGraph extends GraphImpls(Topology.AnyGraphT.reify) {

    object Outbound extends GraphImpls(Topology.AnyGraphT.OutboundT.reify) {}
    type Outbound[V] = Outbound.Graph[V]

  }
  type AnyGraph[V] = AnyGraph.Graph[V]

  object Poset extends GraphImpls(Topology.PosetT.reify) {}
  type Poset[V] = Poset.Graph[V]

  object Semilattice extends GraphImpls(Topology.SemilatticeT.reify) {

    object Upper extends GraphImpls(Topology.SemilatticeT.UpperT.reify) {}
    type Upper[V] = Upper.Graph[V]

  }
  type Semilattice[V] = Semilattice.Graph[V]

  object Tree extends GraphImpls(Topology.TreeT.reify) {

    case class Singleton[V](value: V) extends topologyImpls.Node_[V] {

      final override lazy val inductions: collection.immutable.Nil.type = Nil
    }

    implicit class TreeNodeOps[V](n: topologyImpls.Node_[V]) {

      def mkTree: Tree[V] = Tree.makeExact[V](n)
    }
  }

  type Tree[V] = Tree.Graph[V]
}

object Engine {

  trait HasMaxRecursionDepth {

    def maxDepth: Int
  }

  implicit def engineAsMake(self: Engine): self.AnyGraph.makeTightest.type = self.AnyGraph.makeTightest
}
