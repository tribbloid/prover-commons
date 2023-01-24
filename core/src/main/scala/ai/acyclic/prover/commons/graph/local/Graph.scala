package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy
import ai.acyclic.prover.commons.graph.{Arrow, GraphSystem}
import ai.acyclic.prover.commons.{Correspondence, HasOuter}

import scala.language.implicitConversions

// this is untyped, should always leave the possibility to add dependent type information.
// See project Matryoshka
// OR this article for a possible start:
//  https://macsphere.mcmaster.ca/bitstream/11375/18494/2/thesis.pdf
trait Graph[N] extends GraphSystem.GraphK[N] {

  val outer: Local._GraphType = Graph

  type Ops <: GraphNOps
  protected val Ops: N => Ops

  def roots: Rows[N]
  lazy val rootOps: Rows[Ops] = roots.map(v => nodeOps(v))

  def isEmpty: Boolean = roots.isEmpty

  trait GraphNOps extends Graph._NodeOps[N] with HasOuter {

    def outer = Graph.this

    final lazy val induction: Many[Arrow.Of[N]] = sys.toMany(getInduction)

    final lazy val directEdges = induction.collect {
      case v if v.arrowType.isInstanceOf[Arrow.Edge] => v
    }

    def resolve(): Unit = { induction; nodeText }
  }

  final val nodeOps = Correspondence((v: N) => Ops(v))
}

object Graph extends Local._GraphType {

  trait Outbound[N] extends Graph[N] {
    override val outer: Local._GraphType = Outbound

    trait OutboundNOps extends GraphNOps {

      protected def getInduction: Seq[Arrow.`~>`.Of[N]] = Nil

      final lazy val children: Seq[N] = {
        induction.map(v => v.target)
      }

      lazy val isLeaf: Boolean = children.isEmpty
    }

    type Ops <: OutboundNOps

    def showHierarchyWithRef(
        implicit
        group: LinkedHierarchy#Group
    ): group.Viz[N] = group.Viz(this)
  }

  object Outbound extends Local._GraphType {

    type ArrowUBK[+N] = Arrow.`~>`.Of[N]
  }

  trait _NodeOps[N] {

    val node: N

    protected def getInduction: Seq[Arrow.Of[N]]

    protected def getNodeText: String = node.toString
    final lazy val nodeText: String = getNodeText
  }

  object _NodeOps {

    implicit def unbox[N](ops: _NodeOps[N]): N = ops.node
  }
}
