package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, Lawful}

trait RewriterK[L <: Axiom] extends Lawful.Struct[L] {

  private[this] type NodeV = NodeK.Compat[L, Value]

  def rewrite(src: NodeV)(
      discoverNodes: Seq[NodeV]
  ): NodeV

  object Verified extends RewriterK[L] {

    type Value = RewriterK.this.Value

    val assuming: L = RewriterK.this.assuming

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

  type Aux[X <: Axiom, V] = RewriterK[X] { type Value = V }
  trait Impl[X <: Axiom, V] extends RewriterK[X] { type Value = V }

  case class DoNotRewrite[L <: Axiom, N](override val assuming: L) extends RewriterK[L] {

    type Value = N

    override def rewrite(src: NodeK.Compat[L, Value])(
        discoverNodes: Seq[NodeK.Compat[L, Value]]
    ): NodeK.Compat[L, Value] = src

  }
}
