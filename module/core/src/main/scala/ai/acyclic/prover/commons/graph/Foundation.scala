package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.Axiom

object Foundation {

  trait Lawful extends Foundation0.Lawful {

    type Node[+v] = NodeK.Lt[_Axiom, v]

    type Setter[v] = Setter.Aux[_Axiom, v]
  }

  trait Structure[+X <: Axiom] {

    val axiom: X
    final type _Arrow = axiom._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }

  trait NodeOrGraph[+X <: Axiom.Top] extends Foundation.Structure[X] {

    //  def asGraph: GraphK.Aux[X, Value]
  }

  trait NodeK[+L <: Axiom.Top] extends Priors.Node with Foundation.NodeOrGraph[L] {

    def value: Value

    private[this] type _NodeLt = NodeK.Lt[L, Value]

    def inductions: Seq[(_Arrow, _NodeLt)]

    final lazy val adjacentNodes: Seq[_NodeLt] = inductions.map(_._2)

    object asIterable extends Iterable[Value] {

      override def iterator: Iterator[Value] =
        Iterator(value) ++ adjacentNodes.iterator.flatMap(n => n.asIterable.iterator)
    }
  }

  object NodeK {

    type Aux[+L <: Axiom.Top, V] = NodeK[L] { type Value = V }
    trait Aux_[+L <: Axiom.Top, V] extends NodeK[L] { type Value = V }

    type Lt[+L <: Axiom.Top, +V] = NodeK[L] { type Value <: V }

    implicit class LtView[L <: Axiom.Top, V](self: Lt[L, V]) {

      def map[V2](fn: V => V2): Mapped[L, V, V2] = Mapped[L, V, V2](self, fn)

      def upcast[V2](
          implicit
          ev: V <:< V2
      ): Mapped[L, V, V2] = map((v: V) => v: V2)
    }

    case class Mapped[X <: Axiom.Top, V, V2](
        original: NodeK.Lt[X, V],
        fn: V => V2
    ) extends Aux_[X, V2] {

      override val axiom: original.axiom.type = original.axiom

      override def value: V2 = fn(original.value.asInstanceOf)

      override def nodeText: String = original.nodeText

      override def inductions: Seq[(_Arrow, Mapped[X, V, V2])] = {
        original.inductions.map {
          case (a, n) =>
            a -> Mapped(n, fn)
        }
      }

      override def identityC: Option[Any] = original.identity

      override def evalCacheKeyC: Option[Any] = original.evalCacheKey
    }
  }

  trait GraphK[+X <: Axiom.Top] extends Priors.Graph with Foundation.NodeOrGraph[X] {

    def entries: engine.Batch[NodeK.Lt[X, Value]]
  }

  object GraphK {

    type Aux[+X <: Axiom.Top, V] = GraphK[X] { type Value = V }
    // Acronym of "Less Than"
    type Lt[+X <: Axiom.Top, +V] = GraphK[X] { type Value <: V }
  }

  trait Setter[L <: Axiom.Top] extends Foundation.Structure[L] {

    private type Node = NodeK.Lt[L, Value]

    // TODO: this interface is problematic, should
    def rewrite(src: Node)(
        discoverNodes: Seq[Node]
    ): Node

    object Verified extends Setter[L] {

      val axiom: Setter.this.axiom.type = Setter.this.axiom

      type Value = Setter.this.Value

      override def rewrite(src: Node)(discoverNodes: Seq[Node]): Node = {

        val oldDiscoverNodes = src.adjacentNodes
        if (oldDiscoverNodes == discoverNodes) {
          // no need to rewrite, just return node as-is
          return src
        }

        val result = Setter.this.rewrite(src)(discoverNodes)

        require(
          result.adjacentNodes == discoverNodes,
          s"""Incompatible rewriter?
             |Rewrite result should be [${discoverNodes.mkString(", ")}]
             |but it is actually [${oldDiscoverNodes.mkString(", ")}]""".stripMargin
        )

        result
      }
    }
  }

  object Setter {

    type Aux[X <: Axiom.Top, V] = Setter[X] { type Value = V }
    trait Aux_[X <: Axiom.Top, V] extends Setter[X] { type Value = V }

    case class DoNotRewrite[L <: Axiom.Top, N](
        override val axiom: L
    ) extends Setter[L] {

      type Value = N

      override def rewrite(src: NodeK.Lt[L, Value])(
          discoverNodes: Seq[NodeK.Lt[L, Value]]
      ): NodeK.Lt[L, Value] = src

    }
  }

}
