package ai.acyclic.prover.commons.graph

trait RewriterKind[L <: Law] extends Lawful.Construct[L] {

  type Node = NodeKind.Compat[L, Value]

  def rewrite(src: Node)(
      discoverNodes: Seq[Node]
  ): Node

  object Verified extends RewriterKind[L] {

    type Value = RewriterKind.this.Value

    override def rewrite(src: Node)(discoverNodes: Seq[Node]): Node = {

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

  case class DoNotRewrite[L <: Law, N]() extends RewriterKind[L] {

    type Value = N

    override def rewrite(src: Node)(discoverNodes: Seq[Node]): Node = src
  }
}
