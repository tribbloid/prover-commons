package ai.acyclic.prover.commons.graph

trait RewriterKind[L <: Law] extends Lawful.ConstructKind[L] {

  private[this] type NodeV = NodeKind.Compat[L, Value]

  def rewrite(src: NodeV)(
      discoverNodes: Seq[NodeV]
  ): NodeV

  object Verified extends RewriterKind[L] {

    type Value = RewriterKind.this.Value

    val law: L = RewriterKind.this.law

    override def rewrite(src: NodeV)(discoverNodes: Seq[NodeV]): NodeV = {

      val originalNs = src.discoverNodes
      if (originalNs == discoverNodes) {
        // no need to rewrite, just return node as-is
        return src
      }

      val result = RewriterKind.this.rewrite(src)(discoverNodes)

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

object RewriterKind {

  type Aux[L <: Law, V] = RewriterKind[L] { type Value = V }
  trait AuxEx[L <: Law, V] extends RewriterKind[L] { type Value = V }

  case class DoNotRewrite[L <: Law, N](override val law: L) extends RewriterKind[L] {

    type Value = N

    override def rewrite(src: NodeKind.Compat[L, Value])(
        discoverNodes: Seq[NodeKind.Compat[L, Value]]
    ): NodeKind.Compat[L, Value] = src

  }
}
