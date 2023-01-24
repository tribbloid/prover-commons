package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.HasOuter

trait GraphSystem {

  import GraphSystem._

  type Dataset[T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  type Many[+T] = IndexedSeq[T]
  final def toMany[T](seq: Seq[T]): IndexedSeq[T] = IndexedSeq(seq: _*)

  trait _GraphType extends GraphType {

    final override val outer: GraphSystem.this.type = GraphSystem.this

    // a controversial scala feature prevents this trait from being useful, oops
    private trait _Graph[N] extends GraphK[N] {
      final override val outer: _GraphType.this.type = _GraphType.this
    }
  }
}

object GraphSystem {

  trait GraphType extends HasOuter {

    val outer: GraphSystem

//    type ArrowUB[+N] <: Arrow.Of[N]
  }

  trait _Graph extends HasOuter {

    val outer: GraphType

    lazy val sys: outer.outer.type = outer.outer

    type Many[+T] = sys.Many[T]

    type Rows[T] = sys.Dataset[T]
  }

  trait GraphK[N] extends _Graph {}
}
