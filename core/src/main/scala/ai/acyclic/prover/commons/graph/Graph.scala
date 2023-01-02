package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.viz.HierarchyWithRef

import scala.collection.Factory
import scala.language.implicitConversions

// this is untyped, should always leave the possibility to add dependent type information.
// See this article for a possible start:
//  https://macsphere.mcmaster.ca/bitstream/11375/18494/2/thesis.pdf
object Graph extends GraphSystem {

  trait _Node {

    def resolve(): Unit
  }

  trait Ops[+AA <: ArrowUB] extends _Node {

    def nodeText: String

    def discover: Iterable[AA]

    final lazy val directArrows: Many[AA] = discover.to(Many)
    final lazy val traverse: Many[AA] = directArrows.collect {
      case v if v.arrowType.isInstanceOf[Arrow.Edge] => v
    }

    def resolve(): Unit = directArrows
  }

  type OpsOf[+N <: Node] = Ops[Arrow.Of[N]]

  trait Node extends Ops[Arrow.Of[Node]]

  object Node {

    trait Static {
      self: Node =>

      resolve()
    }
  }

  object Outbound extends GraphSystem {

    type ArrowUBK[+CC <: Node] = Arrow.`~>`.Of[CC]

    trait OpsOf[+CC <: Node] extends Graph.Ops[ArrowUBK[CC]] {

      def outbound: Iterable[ArrowUBK[CC]]

      final override def discover: Iterable[ArrowUBK[CC]] = outbound

      lazy val children: Seq[CC] = {
        directArrows.map(_.target)
      }

      lazy val isLeaf: Boolean = children.isEmpty
    }

    trait UpdateOpsOf[CC <: Node] extends OpsOf[CC] {

      def update(vs: Seq[ArrowUBK[CC]]): CC
    }

    trait Node extends Graph.Node with OpsOf[Node] {

      def showHierarchyWithRef(
          implicit
          format: HierarchyWithRef = HierarchyWithRef.default
      ) = {
        val group = format.Group()
        group.Viz(this)
      }
    }

    object Node {

      implicit def asArrow[N <: Node](v: N): Arrow.`~>`.NoInfo[N] = Arrow.`~>`.NoInfo(v)

      implicit def asArrows[F[t] <: Iterable[t], N <: Node](vs: F[N])(
          implicit
          toF: Factory[Arrow.`~>`.NoInfo[N], F[Arrow.`~>`.NoInfo[N]]]
      ): F[Arrow.`~>`.NoInfo[N]] = {
        val mapped: Iterable[Arrow.`~>`.NoInfo[N]] = vs.map { v =>
          asArrow(v)
        }
        toF.fromSpecific(mapped)
      }
    }
  }
}
