package ai.acyclic.prover.commons.graph



trait GraphSystem {

  type Dataset[T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

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

}
