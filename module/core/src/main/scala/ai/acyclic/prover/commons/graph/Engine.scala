package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, DivergingForm, Topology}

import scala.language.implicitConversions

trait Engine extends Priors.HasBatch {
  self: Singleton =>

  import Engine.*

  type Node[+X <: Axiom.Top, +V] = Foundation.Node[X, V]
  type Setter[X <: Axiom.Top, V] = Foundation.Setter[X, V]

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
        override val axiom: X
    ) extends K[X, V] {}

    case class Transforming[X <: Axiom.Top, V](
        delegate: K[X, V],
        maxRecursionDepth: Int
    ) extends K[X, V] {
      override val axiom: X = delegate.axiom
      override def entries: engine.Batch[Node[X, V]] = {
        delegate.entries
      }
    }
  }

  private def buildFromAxioms[XX <: Axiom.Top, V](
      nodes: Batch[Foundation.Node[XX, V]]
  )(
      assuming: XX
  ): Graph.K[XX, V] =
    Graph.Unchecked[XX, V](nodes)(assuming)

  abstract class GraphType[X <: Axiom.Top](
      val axiom: X // this is a phantom object only used to infer type parameters
  ) extends Foundation.Lawful {

    type _Axiom = X
    type Graph[+V] = Engine.this.Graph[X, V]

    abstract class Plan[V] extends Graph[V] {

      override val axiom: GraphType.this.axiom.type = axiom
    }

    def buildExact[V](
        nodes: Batch[Foundation.Node[X, V]]
    ): Graph[V] =
      buildFromAxioms[X, V](nodes)(GraphType.this.axiom)

    object buildTightest {

      def apply[XX <: X, V](
          nodes: Batch[Foundation.Node[XX, V]]
      )(
          implicit
          assuming: XX
      ): Graph.K[XX, V] =
        buildFromAxioms[XX, V](nodes)(assuming)
    }

    def empty[V]: Graph[V] = makeExact[V]()

    def makeExact[V](
        nodes: Foundation.Node[X, V]*
    ): Graph[V] =
      buildExact[V](parallelize(nodes))

    object makeTightest {

      def apply[XX <: X, V](
          nodes: Foundation.Node[XX, V]*
      )(
          implicit
          assuming: XX
      ): Graph.K[XX, V] =
        buildTightest.apply[XX, V](parallelize(nodes))(assuming)
    }
  }

  implicit def graphTypeAsMake(v: GraphType[?]): v.makeTightest.type = v.makeTightest

  sealed abstract class GraphImpls[X <: Axiom.Top, A <: Arrow](
      val topologyImpls: Topology.Impls[X, A] // this is a phantom object only used to infer type parameters
  ) extends GraphType[X](topologyImpls.concreteAxiom) {

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
    type ArgSetter = Foundation.Setter[X, V]
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

        override val axiom: arg.axiom.type = arg.axiom
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

  object AnyGraph extends GraphImpls(Topology.AnyGraph.reify) {}
  type AnyGraph[V] = AnyGraph.Graph[V]

  object Poset extends GraphImpls(Topology.Poset.reify) {}
  type Poset[V] = Poset.Graph[V]

  object Diverging {

    object Graph extends GraphImpls(DivergingForm.Graph.reify) {}
    type Graph[V] = Graph.Graph[V]

    object Poset extends GraphImpls(DivergingForm.Poset.reify) {}
    type Poset[V] = Poset.Graph[V]

    object UpperSemilattice extends GraphImpls(DivergingForm.UpperSemilattice.reify) {}
    type UpperSemilattice[V] = UpperSemilattice.Graph[V]

    object Tree extends GraphImpls(DivergingForm.Tree.reify) {

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
