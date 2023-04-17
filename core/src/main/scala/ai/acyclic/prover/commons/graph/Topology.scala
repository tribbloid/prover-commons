package ai.acyclic.prover.commons.graph

trait Topology {
  self: Singleton =>

  import Topology._

  type _Arrow <: Arrow

  type CC <: Constraint

  final type _Node[V] = Node[CC, _Arrow, V]

  final type SS[V] = Structure[CC, _Arrow, V]

  trait SimpleDef {
    self: Singleton =>

    type Node <: _Node[SimpleDef.this.Node]

    final type _Graph = Topology.this.SS[Node]
  }

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

  object Rewriter {

    case class DoNotRewrite[N]() extends Rewriter[N] {

      override def rewrite(src: _Node[N])(inductions: Seq[_Node[N]]): _Node[N] = src
    }
  }
}

object Topology {

  trait Constraint

  trait GraphLike extends Topology {
    self: Singleton =>

    override type _Arrow = Arrow
  }

  trait OutboundLike extends Topology {
    self: Singleton =>

    override type _Arrow = Arrow.`~>`.^
  }

  object GraphT extends GraphLike {

    trait CC extends Constraint

    object OutboundT extends OutboundLike {

      trait CC extends GraphT.CC
    }
  }

  object PosetT extends GraphLike {

    trait CC extends GraphT.CC
  }

  object SemilatticeT extends GraphLike {

    trait CC extends PosetT.CC

    object UpperT extends OutboundLike {

      trait CC extends SemilatticeT.CC with GraphT.OutboundT.CC
    }
  }

  object TreeT extends GraphLike {

    trait CC extends SemilatticeT.UpperT.CC
  }
}
