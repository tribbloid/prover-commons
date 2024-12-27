package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.Induction

object Refinement {

  trait Lawful extends Foundations.Lawful {

    type Node[v] = NodeK.Lt[_Axiom, v]

    type Rewriter[v] = RewriterK.Aux[_Axiom, v]
  }

  trait Structure[+X <: Induction] extends Foundations.Lawful {

    override type _Axiom <: X

    val axioms: X
    final type _Arrow = axioms._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }

  trait NodeOrGraph[+X <: Induction] extends Refinement.Structure[X] {

    //  def asGraph: GraphK.Aux[X, Value]
  }

  trait NodeK[+L <: Induction] extends Priors.Node with Refinement.NodeOrGraph[L] {

    def value: Value

    private[this] type NodeLt = NodeK.Lt[L, Value]

    def inductions: Seq[(_Arrow, NodeLt)]

    final lazy val adjacentNodes: Seq[NodeLt] = inductions.map(_._2)

    object asIterable extends Iterable[Value] {

      override def iterator: Iterator[Value] =
        Iterator(value) ++ adjacentNodes.iterator.flatMap(n => n.asIterable.iterator)
    }
  }

  object NodeK {

    type Aux[+L <: Induction, V] = NodeK[L] { type Value = V }
    trait Aux_[+L <: Induction, V] extends NodeK[L] { type Value = V }

    type Lt[+L <: Induction, +V] = NodeK[L] { type Value <: V }

    implicit class LtView[L <: Induction, V](self: Lt[L, V]) {

      def map[V2](fn: V => V2): Mapped[L, V, V2] = Mapped[L, V, V2](self, fn)

      def upcast[V2](
          implicit
          ev: V <:< V2
      ): Mapped[L, V, V2] = map((v: V) => v: V2)
    }

    trait Untyped[+L <: Induction] extends NodeK[L] {
      // actually self typed, but that doesn't convey any extra information

      type Value >: this.type
      final lazy val value: this.type = this
    }

    case class Mapped[+L <: Induction, V, V2](
        original: NodeK.Lt[L, V],
        fn: V => V2
    ) extends Aux_[L, V2] {

      override val axioms: original.axioms.type = original.axioms

      override def value: V2 = fn(original.value.asInstanceOf)

      override def nodeText: String = original.nodeText

      override def inductions: Seq[(_Arrow, Mapped[L, V, V2])] = {
        original.inductions.map {
          case (a, n) =>
            a -> Mapped(n, fn)
        }
      }

      override def identityKeyC: Option[Any] = original.identityKey

      override def evalCacheKeyC: Option[Any] = original.evalCacheKey
    }
  }

  trait GraphK[+X <: Induction] extends Priors.Graph with Refinement.NodeOrGraph[X] {

    type Batch[+_]

    def getEntries: Batch[NodeK.Lt[X, Value]]
  }

  object GraphK {

    type Aux[+X <: Induction, V] = GraphK[X] { type Value = V }
    // Acronym of "Less Than"
    type Lt[+X <: Induction, +V] = GraphK[X] { type Value <: V }
  }

  trait RewriterK[L <: Induction] extends Refinement.Structure[L] {

    private[this] type NodeV = NodeK.Lt[L, Value]

    def rewrite(src: NodeV)(
        discoverNodes: Seq[NodeV]
    ): NodeV

    object Verified extends RewriterK[L] {

      type Value = RewriterK.this.Value

      val axioms: L = RewriterK.this.axioms

      override def rewrite(src: NodeV)(discoverNodes: Seq[NodeV]): NodeV = {

        val oldDiscoverNodes = src.adjacentNodes
        if (oldDiscoverNodes == discoverNodes) {
          // no need to rewrite, just return node as-is
          return src
        }

        val result = RewriterK.this.rewrite(src)(discoverNodes)

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

  object RewriterK {

    type Aux[X <: Induction, V] = RewriterK[X] { type Value = V }
    trait Impl[X <: Induction, V] extends RewriterK[X] { type Value = V }

    case class DoNotRewrite[L <: Induction, N](override val axioms: L) extends RewriterK[L] {

      type Value = N

      override def rewrite(src: NodeK.Lt[L, Value])(
          discoverNodes: Seq[NodeK.Lt[L, Value]]
      ): NodeK.Lt[L, Value] = src

    }
  }

}
