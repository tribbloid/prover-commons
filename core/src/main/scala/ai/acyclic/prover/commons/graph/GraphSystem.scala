package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.{HasOuter, Sameness}

import scala.language.implicitConversions

trait GraphSystem {

  type Dataset[T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  type Many[+T] = IndexedSeq[T]
  final def toMany[T](seq: Seq[T]): IndexedSeq[T] = IndexedSeq(seq: _*)

//  trait _GraphType extends GraphType {
//
////    type ArrowUBK[N] <: Arrow.Of[N]
//
//    final override val outer: GraphSystem.this.type = GraphSystem.this
//
//    // a controversial scala feature prevents this trait from being useful, oops
//    @Deprecated
//    private trait _Graph[N] extends GraphK[N] {
//      final override val graphType: _GraphType.this.type = _GraphType.this
//    }
//  }
}

object GraphSystem {

//  trait GraphType extends HasOuter {
//
//    val outer: GraphSystem
//
////    type ArrowUB[+N] <: Arrow.Of[N]
//  }

  trait _Graph extends HasOuter {

    type NodeType

    val sys: GraphSystem

    final def outer: GraphSystem = sys

    type Many[+T] = sys.Many[T]

    type Rows[T] = sys.Dataset[T]
  }

  trait HasNode[+N] {

    val node: N

    protected def getNodeText: String = node.toString

    final lazy val nodeText: String = getNodeText
  }

  object HasNode {

    implicit def unbox[N](ops: HasNode[N]): N = ops.node
  }

  trait GraphK[N] extends _Graph {

    final type NodeType = N

    trait NodeOps extends InductionBy[Arrow.Of[N]] with HasOuter {

      def outer = GraphK.this
    }

    def roots: Rows[N]

    type Ops <: NodeOps
    protected val Ops: N => Ops

    final val nodeOps = Sameness.ByConstruction.Memoize((v: N) => Ops(v))

    trait InductionBy[+A <: Arrow.Of[N]] {

      protected def getInduction: Seq[A]

      final lazy val induction: Many[A] = sys.toMany(getInduction)

      final lazy val canDiscover: Many[N] = sys.toMany(induction.map(_.target))
    }
  }
}
