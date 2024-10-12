package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axioms, Lawful}

trait RewriterK[L <: Axioms] extends Lawful.Struct[L] {

  private[this] type NodeV = NodeK.Compat[L, Value]

  def rewrite(src: NodeV)(
      discoverNodes: Seq[NodeV]
  ): NodeV

  object Verified extends RewriterK[L] {

    type Value = RewriterK.this.Value

    val axioms: L = RewriterK.this.axioms

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

  type Aux[X <: Axioms, V] = RewriterK[X] { type Value = V }
  trait Impl[X <: Axioms, V] extends RewriterK[X] { type Value = V }

  case class DoNotRewrite[L <: Axioms, N](override val axioms: L) extends RewriterK[L] {

    type Value = N

    override def rewrite(src: NodeK.Compat[L, Value])(
        discoverNodes: Seq[NodeK.Compat[L, Value]]
    ): NodeK.Compat[L, Value] = src

  }
}
