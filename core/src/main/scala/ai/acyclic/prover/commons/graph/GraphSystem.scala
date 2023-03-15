package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.HasOuter

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


}
