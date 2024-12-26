package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Induction, Lawful}

trait RewriterK[X <: Induction] extends Lawful.Refined {

  override type _Axiom = X

//  private[this] type NodeV = NodeV

  def rewrite(src: NodeV)(
      discoverNodes: Seq[NodeV]
  ): NodeV

  object Verified extends RewriterK[X] {

    type Value = RewriterK.this.Value

    override def rewrite(src: NodeV)(discoverNodes: Seq[NodeV]): NodeV = {

      val oldDiscoverNodes = src.discoverNodes
      if (oldDiscoverNodes == discoverNodes) {
        // no need to rewrite, just return node as-is
        return src
      }

      val result = RewriterK.this.rewrite(src)(discoverNodes)

      require(
        result.discoverNodes == discoverNodes,
        s"""Incompatible rewriter?
           |Rewrite result should be [${discoverNodes.mkString(", ")}]
           |but it is actually [${oldDiscoverNodes.mkString(", ")}]""".stripMargin
      )

      result
    }
  }
}

object RewriterK {

  type Aux[X <: Induction, V] = RewriterK[X] { type Value = V }
  trait Aux_[X <: Induction, V] extends RewriterK[X] { type Value = V }

  case class DoNotRewrite[L <: Induction, N]() extends RewriterK[L] {

    type Value = N

    override def rewrite(src: NodeK.Lt[L, Value])(
        discoverNodes: Seq[NodeK.Lt[L, Value]]
    ): NodeK.Lt[L, Value] = src

  }
}
