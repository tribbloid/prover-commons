package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Hasse
import ai.acyclic.prover.commons.graph.{NodeKind, RewriterKind, Topology}

import scala.language.existentials

trait GraphUnary extends Local.Graph.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Graph._L]
  }

  import GraphUnary._

  lazy val distinctEntries: Vector[ArgNode] = argPlan.resolve.entries.distinct

  def diagram_Hasse(
      implicit
      format: Hasse
  ): format.Viz[ArgV] = format.Viz(arg)

  object asIterable extends Iterable[ArgV] {

    override def iterator: Iterator[ArgV] = {
      val base = distinctEntries.iterator

      base.flatMap { bb =>
        bb.asIterable.iterator
      }
    }
  }

//  lazy val asLazyList: LazyList[inputG.Value] = asIterable.to(LazyList)

  case class NodeMap[V2](
      fn: ArgV => V2
  ) extends Arg.PlanEx[V2] {

    override def compute: Arg.GraphLike[V2] = {

      val known: Vector[ArgNode] = distinctEntries

      val result = {

        val newNode: Vector[NodeKind.Aux[ArgLaw, V2]] = known.map { n =>
          n.map(v => fn(v): V2)
        }

        Local.Graph.makeTightest[ArgLaw, V2](newNode: _*)(argPlan.law)
      }

      result
    }
  }

  object NodeUpcast {

    def apply[V2 >: ArgV]: NodeMap[V2] = NodeMap[V2](v => v: V2)
  }

  // TODO:
  //  need to transcribe to a more constraining graph type
  case class Transform(
      rewriter: ArgRewriter,
      maxDepth: Int = Int.MaxValue,
      pruning: Pruning[ArgNode] = identity,
      down: ArgNode => Seq[ArgNode] = v => Seq(v),
      up: ArgNode => Seq[ArgNode] = v => Seq(v)
  ) {

    trait TransformPlan extends Arg.PlanEx[ArgV]

    object DepthFirst extends TransformPlan {

      private def transformInternal(node: ArgNode, depth: Int = maxDepth): Seq[ArgNode] = {
        if (depth > 0) {

          def doTransform(n: ArgNode): Seq[ArgNode] = {
            val downNs: Seq[ArgNode] = down(n)

            val inductionTs: Seq[ArgNode] =
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

      override def compute = {
        val transformed: Seq[ArgNode] = distinctEntries.flatMap(n => transformInternal(n, maxDepth))
        Local.Graph.makeTightest[ArgLaw, ArgV](transformed: _*)(argPlan.law)
      }
    }

    object DepthFirst_Once extends TransformPlan {

      private val delegate = {

        val seen = arg.nodeSameness.Correspondence[ArgNode, ArgNode]()

        Transform(
          rewriter,
          maxDepth,
          pruning = {
            fn =>
              { node =>
                val result: Seq[ArgNode] = seen.get(node) match {
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

      override def compute = {
        delegate.compute
      }
    }

    object DepthFirst_Cached extends TransformPlan {

      private val delegate = {

        val seen = arg.nodeSameness.Correspondence[ArgNode, Seq[ArgNode]]()

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

      override def compute = {
        delegate.compute
      }
    }
  }

  object TransformLinear {
    def apply(
        rewriter: ArgRewriter,
        maxDepth: Int = Int.MaxValue,
        down: ArgNode => ArgNode = v => v,
        pruning: Pruning[ArgNode] = identity,
        up: ArgNode => ArgNode = v => v
    ): Transform = Transform(
      rewriter,
      maxDepth,
      pruning,
      v => Seq(down(v)),
      v => Seq(up(v))
    )
  }

  trait TraversePlan extends Arg.PlanEx[ArgV]

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: ArgNode => Unit = { _: ArgNode => {} },
      up: ArgNode => Unit = { _: ArgNode => {} }
  ) {

    private val delegate = Transform(
      rewriter = RewriterKind.DoNotRewrite(),
      maxDepth,
      down = { v => down(v); Seq(v) },
      up = { v => up(v); Seq(v) }
    )

    object DepthFirst extends TraversePlan {

      override def compute = {

        delegate.DepthFirst.compute
        arg
      }
    }

    object DepthFirst_Once extends TraversePlan {

      override def compute = {

        delegate.DepthFirst_Once.compute
        arg
      }
    }
  }

}

object GraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  case class ^[L <: Local.Graph._L, V](argPlan: LocalEngine.PlanKind.Aux[L, V]) extends GraphUnary {

    override type ArgLaw = L

    override type ArgV = V
  }

  // TODO: memoize all views to avoid duplicated computation
  //  def make[II <: Plan.Lt[Local.Graph[_]]](
  //      override val inputPlan: II
  //  ): ^[arg.OV, TT]
}
