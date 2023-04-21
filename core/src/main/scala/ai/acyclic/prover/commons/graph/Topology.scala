package ai.acyclic.prover.commons.graph

trait Topology {
  self: Singleton =>

  import Topology._

  type A <: Arrow

  type C <: Constraint

  final type Node[V] = NodeKind[C, A, V]

  final type G[V] = GraphKind[C, A, V]

//  case class OnPlatform[P <: Platform](p: P) {
//    trait G[V] extends GraphKind[C, _Arrow, V] {
//      override val platform: P = OnPlatform.this.p
//    }
//  }

  trait SimpleDef {
    self: Singleton =>

    type Node <: Topology.this.Node[SimpleDef.this.Node]

    final type Graph = Topology.this.G[Node]
  }

  trait Rewriter[V] {

    def rewrite(src: Node[V])(
        inductions: Seq[Node[V]]
    ): Node[V]

    object Verified extends Rewriter[V] {

      override def rewrite(src: Node[V])(linksTo: Seq[Node[V]]): Node[V] = {

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

      override def rewrite(src: Node[N])(inductions: Seq[Node[N]]): Node[N] = src
    }
  }
}

object Topology {

  trait Constraint

  trait HasAnyArrow extends Topology {
    self: Singleton =>

    override type A = Arrow
  }

  trait HasOutboundArrow extends Topology {
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

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends HasOutboundArrow {

    trait C extends SemilatticeT.UpperT.C
  }

  private def compileTimeSanityCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< GraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< GraphT.Node[V]]
  }
}
