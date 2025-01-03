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

  abstract class GraphType[X <: Axiom.Top](
      val axiom: X // this is a phantom object only used to infer type parameters
  ) extends Foundation.Lawful {

    type _Axiom = X
    type Graph[+V] = Graph.Lt[X, V]

    abstract class Plan[V] extends Graph.Aux_[X, V] {

      override val axiom: GraphType.this.axiom.type = axiom
    }

    def makeWithAxioms[XX <: X, V](
        nodes: Foundation.NodeK.Lt[XX, V]*
    )(
        assuming: XX
    ): Graph.Unchecked[XX, V] =
      Graph.Unchecked[XX, V](parallelize(nodes))(assuming)

    def makeTightest[XX <: X, V](
        nodes: Foundation.NodeK.Lt[XX, V]*
    )(
        implicit
        assuming: XX
    ): Graph.Unchecked[XX, V] =
      makeWithAxioms[XX, V](nodes*)(assuming)

    def makeExact[V](
        nodes: Foundation.NodeK.Lt[X, V]*
    ): Graph.Unchecked[X, V] =
      makeWithAxioms[X, V](nodes*)(this.axiom)

    def apply[XX <: X, V]( // alias of makeTightest
        nodes: Foundation.NodeK.Lt[XX, V]*
    )(
        implicit
        assuming: XX
    ): Graph.Unchecked[XX, V] = makeTightest[XX, V](nodes*)

    def empty[V]: Graph[V] = makeExact[V]()
  }

  sealed abstract class GraphTypeImpl[X <: Axiom.Top, A <: Arrow](
      val reify: Topology.Reify[X, A] // this is a phantom object only used to infer type parameters
  ) extends GraphType[X](reify.concreteAxiom) {

    type Node_[V] = reify.Node_[V]
    type Setter_[V] = reify.Setter_[V]

    type NodeGroup = reify.NodeGroup

    type Inspection[V] = reify.Inspection[V]
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

  object AnyGraph extends GraphTypeImpl(Topology.AnyGraphT.reify) {

    object Outbound extends GraphTypeImpl(Topology.AnyGraphT.OutboundT.reify) {}
    type Outbound[V] = Outbound.Graph[V]

  }
  type AnyGraph[V] = AnyGraph.Graph[V]

  object Poset extends GraphTypeImpl(Topology.PosetT.reify) {}
  type Poset[V] = Poset.Graph[V]

  object Semilattice extends GraphTypeImpl(Topology.SemilatticeT.reify) {

    object Upper extends GraphTypeImpl(Topology.SemilatticeT.UpperT.reify) {}
    type Upper[V] = Upper.Graph[V]

  }
  type Semilattice[V] = Semilattice.Graph[V]

  object Tree extends GraphTypeImpl(Topology.TreeT.reify) {

    case class Singleton[V](value: V) extends reify.Node_[V] {

      final override lazy val inductions: collection.immutable.Nil.type = Nil
    }

    implicit class TreeNodeOps[V](n: reify.Node_[V]) {

      def mkTree: Tree[V] = Tree.makeExact[V](n)
    }
  }

  type Tree[V] = Tree.Graph[V]
}

object Engine {

  trait HasMaxRecursionDepth {

    def maxDepth: Int
  }
}
