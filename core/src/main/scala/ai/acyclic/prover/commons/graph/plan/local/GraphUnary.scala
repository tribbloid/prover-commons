package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.GraphKind.Top
import ai.acyclic.prover.commons.graph.Topology.GraphT
import ai.acyclic.prover.commons.graph.local.{Graph, Local}
import ai.acyclic.prover.commons.graph.plan.{Plan, PlanArg}

import scala.language.existentials

trait GraphUnary extends PlanArg.Unary {

  type V
  override type LastInputG <: Graph[V]

  import GraphT._
  import GraphUnary._

  case class NodeMap[V2](
      fn: V => V2
  ) extends To[Graph[V2]] {

    override def compute: Graph[V2] = {
      val roots = lastInputG.roots

      Graph(
        roots.map { n =>
          n.map(v => fn(v): V2)
        }: _*
      )
    }
  }

  object NodeUpcast {

    def apply[V2 >: V] = NodeMap[V2](v => v: V2)
  }

  trait TransformPlan extends To[Graph[V]]

  // TODO:
  //  need to transcribe to a more constraining graph type
  case class Transform(
      rewriter: Rewriter[V],
      maxDepth: Int = Int.MaxValue,
      pruning: Pruning[LesserNode[V]] = identity,
      down: LesserNode[V] => Seq[LesserNode[V]] = v => Seq(v),
      up: LesserNode[V] => Seq[LesserNode[V]] = v => Seq(v)
  ) {

    object DepthFirst extends TransformPlan {

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

      override def compute: Graph[V] = {
        val transformed = lastInputG.roots.flatMap(n => transformInternal(n, maxDepth))
        Local.Build(transformed: _*)
      }
    }

    object DepthFirst_Once extends TransformPlan {

      private val delegate = {

        val seen = lastInputG.sameness.Correspondence[LesserNode[V], LesserNode[V]]()

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

      override def compute: Graph[V] = {
        delegate.compute
      }
    }

    object DepthFirst_Cached extends TransformPlan {

      private val delegate = {

        val seen = lastInputG.sameness.Correspondence[LesserNode[V], Seq[LesserNode[V]]]()

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

      override def compute: Graph[V] = {
        delegate.compute
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

  trait TraversePlan extends To[LastInputG] {}

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

    object DepthFirst extends TraversePlan {

      override def compute: LastInputG = {

        delegate.DepthFirst.compute
        lastInputG
      }
    }

    object DepthFirst_Once extends TraversePlan {

//      private val _down = down
//      private val _up = up

      override def compute: LastInputG = {

        delegate.DepthFirst_Once.compute
        lastInputG
      }
    }
  }

}

object GraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  case class ^[VV, IG <: Graph[VV]](
      last: Plan[IG]
  ) extends GraphUnary {
    override type V = VV
    override type LastInputG = IG
  }

  def make[IG <: Top, V](
      arg: Plan[IG]
  )(
      implicit
      ev: IG <:< Graph[V]
      // see https://stackoverflow.com/questions/16291313/scala-inferred-type-arguments-type-bounds-inferring-to-nothing
  ): ^[V, IG with Graph[V]] = {

    new ^[V, IG with Graph[V]](arg.asInstanceOf[Plan[IG with Graph[V]]])
  }
}
