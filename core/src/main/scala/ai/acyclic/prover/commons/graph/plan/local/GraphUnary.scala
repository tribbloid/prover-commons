package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.Topology
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.{Plan, PlanArg}
import ai.acyclic.prover.commons.graph.viz.Hasse

import scala.language.existentials

trait GraphUnary extends PlanArg.Unary {

  import GraphUnary._

  type T = Graph.type

  lazy val entries: Vector[IN] = {
    val result = inputG.entriesC
    val result2 = result
    result2
  }

  lazy val distinctEntries: Vector[IN] = entries.distinct

  def diagram_Hasse(
      implicit
      format: Hasse
  ): format.Viz[IV] = format.Viz(inputG)

  object asIterable extends Iterable[inputG.Value] {

    override def iterator: Iterator[inputG.Value] = {
      val base = entries.iterator

      base.flatMap { bb =>
        bb.asIterable.iterator
      }
    }
  }

  lazy val asLazyList: LazyList[inputG.Value] = asIterable.to(LazyList)

  case class NodeMap[V2](
      fn: IV => V2
  ) extends To[V2] {

    override def compute: IGK[V2] = {
      val known = inputG.entriesC

      Graph(
        known.map { n =>
          n.map(v => fn(v): V2)
        }: _*
      )
    }
  }

  object NodeUpcast {

    def apply[V2 >: IV]: NodeMap[V2] = NodeMap[V2](v => v: V2)
  }

  trait TransformPlan extends To[IV]

  // TODO:
  //  need to transcribe to a more constraining graph type
  case class Transform(
      rewriter: Rewriter,
      maxDepth: Int = Int.MaxValue,
      pruning: Pruning[IN] = identity,
      down: IN => Seq[IN] = v => Seq(v),
      up: IN => Seq[IN] = v => Seq(v)
  ) {

    object DepthFirst extends TransformPlan {

      private def transformInternal(node: IN, depth: Int = maxDepth): Seq[IN] = {
        if (depth > 0) {

          def doTransform(n: IN): Seq[IN] = {
            val downNs: Seq[IN] = down(n)

            val inductionTs: Seq[IN] =
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

      override def compute: IGK[IV] = {
        val ee = entries
        val transformed = ee.flatMap(n => transformInternal(n, maxDepth))
        Graph(transformed: _*)
      }
    }

    object DepthFirst_Once extends TransformPlan {

      private val delegate = {

        val seen = inputG.nodeSameness.Correspondence[ON, ON]()

        Transform(
          rewriter,
          maxDepth,
          pruning = {
            fn =>
              { node =>
                val result: Seq[ON] = seen.get(node) match {
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

      override def compute: IGK[IV] = {
        delegate.compute
      }
    }

    object DepthFirst_Cached extends TransformPlan {

      private val delegate = {

        val seen = inputG.nodeSameness.Correspondence[ON, Seq[ON]]()

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

      override def compute: IGK[IV] = {
        delegate.compute
      }
    }
  }

  object TransformLinear {
    def apply(
        rewriter: Rewriter,
        maxDepth: Int = Int.MaxValue,
        down: IN => IN = v => v,
        pruning: Pruning[IN] = identity,
        up: IN => IN = v => v
    ): Transform = Transform(
      rewriter,
      maxDepth,
      pruning,
      v => Seq(down(v)),
      v => Seq(up(v))
    )
  }

  trait TraversePlan extends To[IG] {}

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: IN => Unit = { _: IN => {} },
      up: IN => Unit = { _: IN => {} }
  ) {

    private val delegate = Transform(
      rewriter = Rewriter.DoNotRewrite[IV](),
      maxDepth,
      down = { v => down(v); Seq(v) },
      up = { v => up(v); Seq(v) }
    )

    object DepthFirst extends TraversePlan {

      override def compute: IG = {

        delegate.DepthFirst.compute
        inputG
      }
    }

    object DepthFirst_Once extends TraversePlan {

      override def compute: IG = {

        delegate.DepthFirst_Once.compute
        inputG
      }
    }
  }

}

object GraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  case class ^[VV, TT <: Topology](
      inputPlan: Plan.Aux[TT]
  ) extends GraphUnary {

    override type IV = VV
  }

  // TODO: memoize all views to avoid duplicated computation
  def make[TT <: Topology](
      arg: Plan.Aux[TT]
  ): ^[arg.OV, TT] = {

    new ^[arg.OV, TT](arg)
  }
}
