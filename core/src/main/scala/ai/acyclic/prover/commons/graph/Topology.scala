package ai.acyclic.prover.commons.graph

trait Topology {
  self: Singleton =>

  import Topology._

  type A <: Arrow

  type C <: Constraint

  final type Node[V] = NodeKind.AuxT[C, A, V]
  final type LesserNode[V] = NodeKind.Lt[C, A, V]

  type G[V] <: GraphKind[C, A, V]

  trait Expression[V] {

    def exe: G[V]

    final lazy val exeOnce: G[V] = exe
  }

  trait System {
    self: Singleton =>

    trait UntypedNode extends NodeKind.Untyped[C, A] {
      self: Node =>

      type Value = Node
    }

    type Node <: UntypedNode

    final type Graph = Topology.this.G[Node]
  }

  trait Rewriter[V] {

    def rewrite(src: LesserNode[V])(
        discoverNodes: Seq[LesserNode[V]]
    ): LesserNode[V]

    object Verified extends Rewriter[V] {

      override def rewrite(src: LesserNode[V])(discoverNodes: Seq[LesserNode[V]]): LesserNode[V] = {

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

      override def rewrite(src: LesserNode[N])(discoverNodes: Seq[LesserNode[N]]): LesserNode[N] = src
    }
  }
}

object Topology {

  trait Constraint

  trait NoEngine extends Topology {
    self: Singleton =>

    type G[V] = GraphKind[C, A, V]
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

    type C = Constraint
  }

  object GraphT extends HasAnyArrow {

    trait C extends Constraint

    object OutboundT extends HasOutboundArrow {

      trait C extends GraphT.C
    }
  }

  object PosetT extends HasAnyArrow {

    trait C extends GraphT.C
  }

  object SemilatticeT extends HasAnyArrow {

    trait C extends PosetT.C

    object UpperT extends HasOutboundArrow {

      trait C extends SemilatticeT.C with GraphT.OutboundT.C

      implicit class NodeOps[V](n: LesserNode[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends HasOutboundArrow {

    trait C extends SemilatticeT.UpperT.C
  }

  private def compileTimeSanityCheck[V](): Unit = {

    implicitly[PosetT.LesserNode[Int] <:< GraphT.LesserNode[Int]]

    implicitly[PosetT.LesserNode[V] <:< GraphT.LesserNode[V]]
  }
}
