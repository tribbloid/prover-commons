package ai.acyclic.prover.commons.graph

trait Topology {
  self: Singleton =>

  import Topology._

  type A <: Arrow

  type L <: Law

  type NodeEx[V] = NodeKind.AuxEx[L, A, V]
  type Node[V] = NodeKind.Lt[L, A, V]

  type G[V] <: GraphKind.Aux[L, A, V]

  trait Expression[V] {

    def exe: G[V]

    final lazy val exeOnce: G[V] = exe
  }

  trait Untyped {
    self: Singleton =>

    trait UntypedNode extends NodeKind.Untyped[L, A] {
      self: Node =>

      type Value = Node
    }

    type Node <: UntypedNode

    final type Graph = Topology.this.G[Node]
  }

  trait Rewriter[V] {

    def rewrite(src: Node[V])(
        discoverNodes: Seq[Node[V]]
    ): Node[V]

    object Verified extends Rewriter[V] {

      override def rewrite(src: Node[V])(discoverNodes: Seq[Node[V]]): Node[V] = {

        val originalNs = src.discoverNodes
        if (originalNs == discoverNodes) {
          // no need to rewrite, just return node as-is
          return src
        }

        val result = Rewriter.this.rewrite(src)(discoverNodes)

        require(
          result.discoverNodes == discoverNodes,
          s"""Incompatible rewriter?
             |Rewrite result should be [${discoverNodes.mkString(", ")}]
             |but it is actually [${originalNs.mkString(", ")}]""".stripMargin
        )

        result
      }
    }
  }

  object Rewriter {

    case class DoNotRewrite[N]() extends Rewriter[N] {

      override def rewrite(src: Node[N])(discoverNodes: Seq[Node[N]]): Node[N] = src
    }
  }
}

object Topology {

  trait Law

  trait NoEngine extends Topology {
    self: Singleton =>

    type G[V] = GraphKind.Aux[L, A, V]
  }

  trait HasAnyArrow extends NoEngine {
    self: Singleton =>

    override type A = Arrow
  }

  trait HasOutboundArrow extends NoEngine {
    self: Singleton =>

    override type A = Arrow.`~>`.^
  }

  object AnyT extends HasAnyArrow {

    type L = Law
  }

  object GraphT extends HasAnyArrow {

    trait L extends Law

    object OutboundT extends HasOutboundArrow {

      trait L extends GraphT.L
    }
  }

  object PosetT extends HasAnyArrow {

    trait L extends GraphT.L
  }

  object SemilatticeT extends HasAnyArrow {

    trait L extends PosetT.L

    object UpperT extends HasOutboundArrow {

      trait L extends SemilatticeT.L with GraphT.OutboundT.L

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends HasOutboundArrow {

    trait L extends SemilatticeT.UpperT.L
  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< GraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< GraphT.Node[V]]
  }
}
