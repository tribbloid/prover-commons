package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.law.Law

trait RewriterK[L <: Law] extends Lawful.Construct[L] {

  private[this] type NodeV = NodeK.Compat[L, Value]

  def rewrite(src: NodeV)(
      discoverNodes: Seq[NodeV]
  ): NodeV

  object Verified extends RewriterK[L] {

    type Value = RewriterK.this.Value

    val law: L = RewriterK.this.law

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

  type Aux[L <: Law, V] = RewriterK[L] { type Value = V }
  trait AuxEx[L <: Law, V] extends RewriterK[L] { type Value = V }

  case class DoNotRewrite[L <: Law, N](override val law: L) extends RewriterK[L] {

    type Value = N

    override def rewrite(src: NodeK.Compat[L, Value])(
        discoverNodes: Seq[NodeK.Compat[L, Value]]
    ): NodeK.Compat[L, Value] = src

  }
}
