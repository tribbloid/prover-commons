package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.GraphKind.Like
import ai.acyclic.prover.commons.graph.Topology.GraphT
import ai.acyclic.prover.commons.graph.local.{Graph, Local}
import ai.acyclic.prover.commons.graph.plan.{Arity, Expression}
import shapeless.Sized

import scala.language.existentials

case class GraphUnary[IG <: Graph[V], V] private (
    arg: Expression[IG]
) extends Arity.Unary.Expressions[IG] {

  import GraphUnary._
  import GraphT._

  final override lazy val args = Sized(arg)

  lazy val inputGraph = arg.exeOnce

  case class UpcastNode[V2 >: V]() extends To[Graph[V2]] {

    override def exe: Graph[V2] = {
      val roots = inputGraph.roots

      Graph(
        roots.map { n =>
          n.map(v => v: V2)
        }: _*
      )
    }
  }

  trait TransformLike extends To[Graph[V]]

  // TODO:
  //  need to transcribe to a different graph type
  case class Transform(
                        rewriter: Rewriter[V],
                        maxDepth: Int = Int.MaxValue,
                        pruning: Pruning[LesserNode[V]] = identity,
                        down: LesserNode[V] => Seq[LesserNode[V]] = v => Seq(v),
                        up: LesserNode[V] => Seq[LesserNode[V]] = v => Seq(v)
  ) {

    object DepthFirst extends TransformLike {

      private def transformInternal(node: LesserNode[V], depth: Int = maxDepth): Seq[LesserNode[V]] = {
        if (depth > 0) {

          def doTransform(n: LesserNode[V]): Seq[LesserNode[V]] = {
            val downNs: Seq[LesserNode[V]] = down(n)

            val inductionTs: Seq[LesserNode[V]] =
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

        val seen = inputGraph.sameness.Correspondence[LesserNode[V], LesserNode[V]]()

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

        val seen = inputGraph.sameness.Correspondence[LesserNode[V], Seq[LesserNode[V]]]()

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
               down: LesserNode[V] => LesserNode[V] = v => v,
               pruning: Pruning[LesserNode[V]] = identity,
               up: LesserNode[V] => LesserNode[V] = v => v
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
                       down: LesserNode[V] => Unit = { _: LesserNode[V] => {} },
                       up: LesserNode[V] => Unit = { _: LesserNode[V] => {} }
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
