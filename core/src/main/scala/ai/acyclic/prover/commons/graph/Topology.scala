package ai.acyclic.prover.commons.graph

trait Topology[A <: Arrow] {

  type _NodeBound[V] = Node[V, A, _Node[V]]

  type _Node[V] <: _NodeBound[V]

  

  type _Graph[V] = GraphK[_Node[V]]

  trait Rewriter[V] {

    def rewrite(src: _Node[V])(
        inductions: Seq[_Node[V]]
    ): _Node[V]

    object Verified extends Rewriter[V] {

      override def rewrite(src: _Node[V])(linksTo: Seq[_Node[V]]): _Node[V] = {

        val originalNs = src.discoverNodes
        if (originalNs == linksTo) {
          // no need to rewrite, just return node as-is
          return src
        }

        val result = Rewriter.this.rewrite(src)(linksTo)

        require(
          originalNs == linksTo,
          s"""Incompatible rewriter?
             |Rewrite result should be [${linksTo.mkString(", ")}]
             |but it is actually [${originalNs.mkString(", ")}]""".stripMargin
        )

        result
      }
    }
  }
}

object Topology {

  object GraphT extends Topology[Arrow] {

    trait _Node[V] extends _NodeBound[V] {

      final lazy val edgeInduction = valueInduction.collect {
        case v if v._1.arrowType.isInstanceOf[Arrow.Edge] => v
      }

      def resolve(): Unit = {
        valueInduction;
        nodeText
      }
    }

    object OutboundT extends Topology[Arrow.`~>`.^] {

      trait _Node[V] extends GraphT._Node[V] with _NodeBound[V] {

        final lazy val children: Seq[V] = {
          valueInduction.map(v => v._2)
        }

        lazy val isLeaf: Boolean = children.isEmpty
      }
    }
  }

  object PosetT extends Topology[Arrow] {

    trait _Node[V] extends GraphT._Node[V] with _NodeBound[V] {}
  }

  object SemilatticeT extends Topology[Arrow] {

    trait _Node[V] extends PosetT._Node[V] with _NodeBound[V] {}

    object UpperT extends Topology[Arrow.`~>`.^] {

      trait _Node[V] extends _NodeBound[V] with SemilatticeT._Node[V] with GraphT.OutboundT._Node[V] {

        //      def ops: Node => Impl

        //      lazy val allOffsprings: Seq[Node] = {
        //        val cc = children
        //        val others = children.flatMap { child =>
        //          ops(child).allOffsprings
        //        }
        //        cc ++ others
        //      }
      }
    }
  }

  object TreeT extends Topology[Arrow.`~>`.^] {

    trait _Node[V] extends SemilatticeT.UpperT._Node[V] with _NodeBound[V] {}
  }

}
