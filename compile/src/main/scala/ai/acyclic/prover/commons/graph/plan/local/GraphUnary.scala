package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.GraphKind.Like
import ai.acyclic.prover.commons.graph.Topology.GraphT
import ai.acyclic.prover.commons.graph.local.{Graph, Local}
import ai.acyclic.prover.commons.graph.plan.{Arity, Expression}
import shapeless.Sized

case class GraphUnary[IG <: Graph[V], V] private (
    arg: Expression[IG]
) extends Arity.Unary.Expressions[IG] {

  import GraphUnary._
  import GraphT._

  final override lazy val args = Sized(arg)

  lazy val inputGraph = arg.exeOnce

  case class UpcastNode[V2 >: V]() extends To[Graph[V2]] {

    override def exe: Graph[V2] = {
      val roots: Local.Dataset[inputGraph.Node] = inputGraph.roots

      Graph(
        roots.map { n =>
          n.map(v => v: V2)
        }: _*
      )
    }
  }

  trait TransformLike extends To[Graph[V]]

  // TODO:
  //  need to cross NodeType
  //  need to transcribe to a different graph type
  case class Transform(
      rewriter: Rewriter[V],
      maxDepth: Int = Int.MaxValue,
      pruning: Pruning[Node[V]] = identity,
      down: Node[V] => Seq[Node[V]] = v => Seq(v),
      up: Node[V] => Seq[Node[V]] = v => Seq(v)
  ) {

    object DepthFirst extends TransformLike {

      private def transformInternal(node: Node[V], depth: Int = maxDepth): Seq[Node[V]] = {
        if (depth > 0) {

          def doTransform(n: Node[V]): Seq[Node[V]] = {
            val downNs: Seq[Node[V]] = down(n)

            val inductionTs: Seq[Node[V]] =
              downNs.map { n =>
                val successors = n.discoverNodes
                val successorsTransformed = successors.flatMap { nn =>
                  transformInternal(nn, depth - 1)
                }
                val rewritten = rewriter.Verified.rewrite(n)(successorsTransformed)
                rewritten
              }

            val results = inductionTs.flatMap { n =>
              up(n)
            }

            results
          }

          pruning(doTransform).apply(node)
        } else {
          Seq(node)
        }
      }

      override def exe: Graph[V] = {
        val transformed = inputGraph.roots.flatMap(n => transformInternal(n, maxDepth))
        Local.Build(transformed: _*)
      }
    }

    object DepthFirst_Once extends TransformLike {

      private val delegate = {

        val seen = inputGraph.sameness.Correspondence[Node[V], Node[V]]()

        Transform(
          rewriter,
          maxDepth,
          pruning = {
            fn =>
              { node =>
                val result = seen.get(node) match {
                  case Some(n) =>
                    Seq(n)
                  case None =>
                    seen.getOrElseUpdate(node, () => node)
                    fn(node)
                }

                result
              }
          },
          down,
          up
        ).DepthFirst
      }

      override def exe: Graph[V] = {
        delegate.exe
      }
    }

    object DepthFirst_Cached extends TransformLike {

      private val delegate = {

        val seen = inputGraph.sameness.Correspondence[Node[V], Seq[Node[V]]]()

        Transform(
          rewriter,
          maxDepth,
          pruning = {
            fn =>
              { node =>
                val result = seen.get(node) match {
                  case Some(r) =>
                    r
                  case None =>
                    val result = fn(node)
                    seen.getOrElseUpdate(node, () => result)
                    result
                }

                result
              }
          },
          down,
          up
        ).DepthFirst
      }

      override def exe: Graph[V] = {
        delegate.exe
      }
    }
  }

  object TransformLinear {
    def apply(
        rewriter: Rewriter[V],
        maxDepth: Int = Int.MaxValue,
        down: Node[V] => Node[V] = v => v,
        pruning: Pruning[Node[V]] = identity,
        up: Node[V] => Node[V] = v => v
    ): Transform = Transform(
      rewriter,
      maxDepth,
      pruning,
      v => Seq(down(v)),
      v => Seq(up(v))
    )
  }

  trait TraverseLike extends To[IG] {}

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: Node[V] => Unit = { _: Node[V] => {} },
      up: Node[V] => Unit = { _: Node[V] => {} }
  ) {

    private val delegate = Transform(
      rewriter = Rewriter.DoNotRewrite[V](),
      maxDepth,
      down = { v => down(v); Seq(v) },
      up = { v => up(v); Seq(v) }
    )

    object DepthFirst extends TraverseLike {

      override def exe: IG = {

        delegate.DepthFirst.exe
        inputGraph
      }
    }

    object DepthFirst_Once extends TraverseLike {

//      private val _down = down
//      private val _up = up

      override def exe: IG = {

        delegate.DepthFirst_Once.exe
        inputGraph
      }
    }
  }

  // TODO: delete the following example, note sure if map or monadic flatMap can be supported
  //  case class TransformNode[G1 <: Graph[Int], G2 <: Graph[String]](original: G1) {
  //
  //    // just a carbon copy:
  //    lazy val g1: G1 = ???
  //
  //    lazy val rewriter: Transcriber[String, G2] = ???
  //
  //    def mapNodeFn: (Int => String) = _.toString
  //
  //    lazy val compile: G2 = {
  //
  //      def _internal(oldNode: Int): String = {
  //        val oldInductions: Seq[Arrow.Of[Int]] = g1.nodeOps(oldNode).induction
  //
  //        val mappedNode: String = mapNodeFn(oldNode)
  ////        val mappedInductions: String =
  //
  //        val rewrittenInductions = oldInductions.map { arrow =>
  //          val target = _internal(arrow.target)
  //          val newArrow = rewriter.rewriteArrow(arrow).setTarget(target)
  //          newArrow
  //        }
  //
  //        val newNode: String = rewriter.rewriteNode(mappedNode).setInductions(rewrittenInductions)
  //
  //        newNode
  //      }
  //
  //      val newRoots = original.roots.map { root =>
  //        _internal(root)
  //      }
  //
  //      rewriter.build(newRoots)
  //    }
  //  }

}

object GraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  def make[IG <: Like, N](
      arg: Expression[IG]
  )(
      implicit
      ev: IG <:< Graph[N]
      // see https://stackoverflow.com/questions/16291313/scala-inferred-type-arguments-type-bounds-inferring-to-nothing
  ): GraphUnary[IG with Graph[N], N] = {

    new GraphUnary[IG with Graph[N], N](arg.asInstanceOf[Expression[IG with Graph[N]]])
  }
}
