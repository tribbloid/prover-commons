package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.Foundation.Graph.K
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.VisualOps
import ai.acyclic.prover.commons.graph.topology.Axiom

object Foundation {

  trait Lawful extends Foundation0.Lawful {

    type Node[+v] = Foundation.Node.K[_Axiom, v]

    type Setter[v] = Foundation.Updater[_Axiom, v]
  }

  trait Structure[
      +X <: Axiom.Top,
      +V // fixed-point bound: type of values of this node and all its descendants, NOT the type of node value
  ] {

    val axiom: X
    final type _Arrow = axiom._Arrow

//    private type _Node = Node[X, V] TODO: how to compile them?
//    private type _Setter = Setter[X, V]
//    private type _Graph = Graph[X, V]
  }

  trait NodeOrGraph[+X <: Axiom.Top, +V] extends Foundation.Structure[X, V] {}

  type Node[+X <: Axiom.Top, +V] = Node.K[X, V]
  object Node {

    trait K[+X <: Axiom.Top, +V] extends Priors.Node with Foundation.NodeOrGraph[X, V] {

      def value: V

      // TODO: should this be an IndexedSeq to rule out lazy execution?
      def inductions: Seq[(_Arrow, Node.K[X, V])]

      final lazy val inductionNodes: Seq[Node.K[X, V]] = inductions.map(_._2)

      final lazy val inductionValues: Seq[V] = inductionNodes.map(_.value)
    }

    implicit class LtView[L <: Axiom.Top, V](self: Node[L, V]) {

      def map[V2](fn: V => V2): Mapped[L, V, V2] = Mapped[L, V, V2](self, fn)

      def upcast[V2](
          implicit
          ev: V <:< V2
      ): Mapped[L, V, V2] = map((v: V) => v: V2)
    }

    case class Mapped[X <: Axiom.Top, V, V2](
        original: Node[X, V],
        fn: V => V2
    ) extends Node[X, V2] {

      override val axiom: original.axiom.type = original.axiom

      override def value: V2 = fn(original.value.asInstanceOf)

      override def nodeText: String = original.nodeText

      override def inductions: Seq[(_Arrow, Mapped[X, V, V2])] = {
        original.inductions.map {
          case (a, n) =>
            a -> Mapped(n, fn)
        }
      }

      override lazy val identity: Option[Any] = original.identity

      override lazy val evalCacheKey: Option[Any] = original.evalCacheKey
    }
  }

  type Graph[+X <: Axiom.Top, +V] = K[X, V]
  object Graph {

    trait K[+X <: Axiom.Top, +V] extends Priors.Graph with Foundation.NodeOrGraph[X, V] {

      def entries: engine.Batch[Node[X, V]]
    }

    type Lt[+X <: Axiom.Top, +V] = K[X, V]
  }

  type Updater[X <: Axiom.Top, V] = Updater.K[X, V]
  object Updater {

    trait K[X <: Axiom.Top, V] extends Foundation.Structure[X, V] {

      private type _Node = Node[X, V]

      def update(src: _Node)(
          newInduction: Seq[_Node] // TODO: should be Seq of pair tuples
      ): _Node

      object Verified extends K[X, V] {

        val axiom: K.this.axiom.type = K.this.axiom

        override def update(src: _Node)(newInduction: Seq[_Node]): _Node = {

          val oldDiscoverNodes = src.inductionNodes
          if (oldDiscoverNodes == newInduction) {
            // no need to rewrite, just return node as-is
            return src
          }

          val result = K.this.update(src)(newInduction)

          require(
            result.inductionNodes == newInduction,
            s"""Incompatible rewriter?
               |Rewrite result should be [${newInduction.mkString(", ")}]
               |but it is actually [${oldDiscoverNodes.mkString(", ")}]""".stripMargin
          )

          result
        }
      }
    }

    case class DoNotRewrite[X <: Axiom.Top, V](
        override val axiom: X
    ) extends K[X, V] {

      override def update(src: Node[X, V])(
          newInduction: Seq[Node[X, V]]
      ): Node[X, V] = src

    }
  }

  implicit class showGraph[X <: Axiom.Top, V](graph: Local.Graph[X, V]) extends VisualOps[X, V](graph) {}

  implicit class showNode[X <: Axiom.Top, V](node: Foundation.this.Node[X, V])(
      implicit
      assuming: X
  ) extends VisualOps[X, V](
        Local[X, V](node)
      ) {}
}
