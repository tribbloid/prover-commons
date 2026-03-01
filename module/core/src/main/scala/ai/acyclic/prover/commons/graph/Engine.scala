package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, DivergingForm, Topology}

import scala.language.implicitConversions

trait Engine extends Priors.HasBatch {
  self: Singleton =>

  import Engine.*

  type Node[+X <: Axiom.Top, +V] = Foundation.Node[X, V]
  type Setter[X <: Axiom.Top, V] = Foundation.Updater[X, V]

  type Graph[+X <: Axiom.Top, +V] = Graph.K[X, V]
  object Graph {

    trait K[+X <: Axiom.Top, +V] extends Foundation.Graph[X, V] {

      final override lazy val engine: Engine.this.type = Engine.this

      def withMaxRecursionDepth(maxRecursionDepth: Int): K[X, V] = {
        Graph.Transforming(this, maxRecursionDepth)
      }

      def toX[_X >: X <: Axiom.Top]: K[_X, V] = this
      def toV[_V >: V]: K[X, _V] = this

      // --- from Ops

      def isEmpty: Boolean = entries.isEmpty

      lazy val distinctEntries: Batch[Node[X, V]] = entries.distinct

      def collectAllNodes: LazyList[Node[X, V]] = {

        val base = distinctEntries.collect.to(LazyList)

        base
          .flatMap { bb =>
            bb.inductionNodes
          }
          .to(LazyList)
      }

      def collectAll: LazyList[V] = {

        collectAllNodes.map(_.value)
      }
    }

    /**
      * Graph representation without any validation
      */
    case class Unchecked[X <: Axiom.Top, V](
        entries: Batch[Foundation.Node[X, V]]
    )(
        override val topology: Topology.Lt[X, Arrow]
    ) extends K[X, V] {
      override type _Axiom = X
      override type _Arrow = Arrow
    }

    case class Transforming[X <: Axiom.Top, V](
        delegate: K[X, V],
        maxRecursionDepth: Int
    ) extends K[X, V] {
      override type _Axiom = delegate._Axiom
      override type _Arrow = delegate._Arrow
      override val topology: Topology.Lt[_Axiom, _Arrow] = delegate.topology
      override def entries: engine.Batch[Node[X, V]] = {
        delegate.entries
      }
    }
  }

  private def buildFromTopology[XX <: Axiom.Top, V](
      nodes: Batch[Foundation.Node[XX, V]]
  )(
      topology: Topology.Lt[XX, Arrow]
  ): Graph.K[XX, V] =
    Graph.Unchecked[XX, V](nodes)(topology)

  private def buildFromNodes[XX <: Axiom.Top, V](
      nodes: Batch[Foundation.Node[XX, V]]
  ): Graph.K[XX, V] = {
    val topology = nodes.collect.headOption
      .map(_.topology)
      .getOrElse(
        throw new IllegalArgumentException(
          "Cannot infer topology from empty node list; use makeExact/buildExact instead."
        )
      )
    buildFromTopology[XX, V](nodes)(topology)
  }

  abstract class GraphType[TT <: Topology](
      val topology: TT // this is a phantom object only used to infer type parameters
  ) extends Foundation.Lawful {

    type _Axiom = topology._Axiom
    type _Arrow = topology._Arrow
    type Graph[+V] = Engine.this.Graph[topology._Axiom, V]

    abstract class Plan[V] extends Graph[V] {

      override type _Axiom = GraphType.this._Axiom
      override type _Arrow = GraphType.this._Arrow
      override val topology: Topology.Lt[_Axiom, _Arrow] = GraphType.this.topology
    }

    def buildExact[V](
        nodes: Batch[Foundation.Node[_Axiom, V]]
    ): Graph[V] =
      buildFromTopology[_Axiom, V](nodes)(GraphType.this.topology)

    object buildTightest {

      def apply[XX <: _Axiom, V](
          nodes: Batch[Foundation.Node[XX, V]]
      ): Graph.K[XX, V] =
        buildFromNodes[XX, V](nodes)
    }

    def empty[V]: Graph[V] = makeExact[V]()

    def makeExact[V](
        nodes: Foundation.Node[_Axiom, V]*
    ): Graph[V] =
      buildExact[V](parallelize(nodes))

    object makeTightest {

      def apply[XX <: _Axiom, V](
          nodes: Foundation.Node[XX, V]*
      ): Graph.K[XX, V] =
        buildTightest.apply[XX, V](parallelize(nodes))
    }
  }

  implicit def graphTypeAsMake(v: GraphType[?]): v.makeTightest.type = v.makeTightest

  sealed abstract class GraphImpls[TT <: Topology](
      val topologyImpls: TT // this is a phantom object only used to infer type parameters
  ) extends GraphType[TT](topologyImpls) {

    type Node_[V] = topologyImpls.Node_[V]
    type Setter_[V] = topologyImpls.Setter_[V]

    type Codomain = topologyImpls.Codomain

    type Inspection[V] = topologyImpls.Inspection[V]
  }

  trait Ops[
      X <: Axiom.Top,
      V
  ] {

    type Prev
    val prev: Prev

    type ArgNode = Foundation.Node[X, V]
    type ArgSetter = Foundation.Updater[X, V]
  }

  object Ops {

    abstract class Unary[
        X <: Axiom.Top,
        V
    ](
        val arg: Graph[X, V]
    ) extends Ops[X, V]
        with HasMaxRecursionDepth {

      type MaxGraph <: Axiom.Top

      final lazy val maxRecursionDepth: Int = {
        arg match {
          case Graph.Transforming(_, d) => d
          case _                        => HasMaxRecursionDepth.Default.maxRecursionDepth
        }
      }

      type Prev = Unit
      val prev: Unit = {}

      abstract class Plan[VV] extends Graph[X, VV] {

        override type _Axiom = arg._Axiom
        override type _Arrow = arg._Arrow
        override val topology: Topology.Lt[_Axiom, _Arrow] = arg.topology
      }
    }

    abstract class Binary[
        X <: Axiom.Top,
        V
    ](
        val arg: Graph[X, V]
    ) extends Ops[X, V] {

      type Prev = Unary[X, V]

      final def maxRecursionDepth: Int = {
        prev.maxRecursionDepth
      }
    }
  }

  object AnyGraph extends GraphImpls[Topology.AnyGraph.type](Topology.AnyGraph) {}
  type AnyGraph[V] = AnyGraph.Graph[V]

  object Poset extends GraphImpls[Topology.Poset.type](Topology.Poset) {}
  type Poset[V] = Poset.Graph[V]

  object Diverging {

    object Graph extends GraphImpls[DivergingForm.Graph.type](DivergingForm.Graph) {}
    type Graph[V] = Graph.Graph[V]

    object Poset extends GraphImpls[DivergingForm.Poset.type](DivergingForm.Poset) {}
    type Poset[V] = Poset.Graph[V]

    object UpperSemilattice
        extends GraphImpls[DivergingForm.UpperSemilattice.type](DivergingForm.UpperSemilattice) {}
    type UpperSemilattice[V] = UpperSemilattice.Graph[V]

    object Tree extends GraphImpls[DivergingForm.Tree.type](DivergingForm.Tree) {

      case class Singleton[V](value: V) extends topologyImpls.Node_[V] {

        final override lazy val inductions: collection.immutable.Nil.type = Nil
      }

      implicit class TreeNodeOps[V](n: topologyImpls.Node_[V]) {

        def mkTree: Tree[V] = Tree.makeExact[V](n)
      }
    }
    type Tree[V] = Tree.Graph[V]
  }

}

object Engine {

  object HasMaxRecursionDepth { // TODO: name too long

    object Default extends HasMaxRecursionDepth {
      override def maxRecursionDepth: Int = 10
    }
  }

  trait HasMaxRecursionDepth {

    def maxRecursionDepth: Int
  }

  implicit def engineAsMake(self: Engine): self.AnyGraph.makeTightest.type = self.AnyGraph.makeTightest
}
