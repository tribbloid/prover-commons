package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.RewriterK
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Flow
import ai.acyclic.prover.commons.same.Same

trait AnyGraphUnary extends Local.AnyGraph.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.AnyGraph._Axiom]
  }

  import AnyGraphUnary.*

  def isEmpty: Boolean = arg.entries.isEmpty

  lazy val distinctEntries: Vector[ArgNode] = arg.entries.distinct

  def text_flow(
      implicit
      format: Flow
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
  ) extends Arg.PlanImpl[V2] {

    override def compute: Arg.Graph[V2] = {

      val known: Vector[ArgNode] = distinctEntries

      val result = {

        val newNode = known.map { n =>
          n.map(v => fn(v): V2)
        }

        Local.AnyGraph.makeWithAxioms[ArgLaw, V2](newNode *)(argPlan.axioms)
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
      pruning: Pruning[ArgNode] = identity,
      down: ArgNode => Seq[ArgNode] = v => Seq(v),
      up: ArgNode => Seq[ArgNode] = v => Seq(v)
  ) {

    trait TransformPlan extends Arg.PlanImpl[ArgV]

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

      override def compute: LocalEngine.GraphKOfTheEngine.Unchecked[ArgLaw, ArgV] = {
        val transformed: Seq[ArgNode] = distinctEntries.flatMap(n => transformInternal(n, maxDepth))
        Local.AnyGraph.makeWithAxioms[ArgLaw, ArgV](transformed *)(argPlan.axioms)
      }
    }

    object DepthFirst_Once extends TransformPlan {

      private val delegate = {

        val evaled = Same.Native.Lookup[Any, ArgNode]()

        Transform(
          rewriter,
          pruning = { fn =>
            { node =>
              val keyOpt = node.evalCacheKey

              val result: Seq[ArgNode] = keyOpt.flatMap { key =>
                evaled.get(key)
              } match {
                case Some(n) =>
                  Seq(n)
                case None =>
                  keyOpt.foreach { key =>
                    evaled.getOrElseUpdateOnce(key)(node)
                  }

                  fn(node)
              }

              result
            }
          },
          down,
          up
        ).DepthFirst
      }

      override def compute: ai.acyclic.prover.commons.graph.local.LocalEngine.GraphKOfTheEngine.Unchecked[
        AnyGraphUnary.this.ArgLaw,
        AnyGraphUnary.this.ArgV
      ] = {
        delegate.compute
      }
    }

    object DepthFirst_Cached extends TransformPlan {

      private val delegate = {

        val evaled = Same.Native.Lookup[Any, Seq[ArgNode]]()

        Transform(
          rewriter,
          pruning = { fn =>
            { node =>
              val keyOpt = node.evalCacheKey

              val result = keyOpt.flatMap { key =>
                evaled.get(key)
              } match {
                case Some(r) =>
                  r
                case None =>
                  val result = fn(node)
                  keyOpt.foreach { key =>
                    evaled.getOrElseUpdateOnce(key)(result)
                  }
                  result
              }

              result
            }
          },
          down,
          up
        ).DepthFirst
      }

      override def compute: ai.acyclic.prover.commons.graph.local.LocalEngine.GraphKOfTheEngine.Unchecked[
        AnyGraphUnary.this.ArgLaw,
        AnyGraphUnary.this.ArgV
      ] = {
        delegate.compute
      }
    }
  }

  object TransformLinear {
    def apply(
        rewriter: ArgRewriter,
        down: ArgNode => ArgNode = v => v,
        pruning: Pruning[ArgNode] = identity,
        up: ArgNode => ArgNode = v => v
    ): Transform = Transform(
      rewriter,
      pruning,
      v => Seq(down(v)),
      v => Seq(up(v))
    )
  }

  trait TraversePlan extends Arg.PlanImpl[ArgV]

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      down: ArgNode => Unit = { (_: ArgNode) => {} },
      up: ArgNode => Unit = { (_: ArgNode) => {} }
  ) {

    private val delegate = Transform(
      rewriter = RewriterK.DoNotRewrite(arg.axioms),
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

object AnyGraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  case class ^[L <: Local.AnyGraph._Axiom, V](
      argPlan: LocalEngine.PlanK.Compat[L, V],
      override val maxDepth: Int = 20
  ) extends AnyGraphUnary {

    override type ArgLaw = L

    override type ArgV = V

    def &&[L2 <: Local.AnyGraph._Axiom, V2](
        argPlan: LocalEngine.PlanK.Compat[L2, V2],
        maxDepth: Int = ^.this.maxDepth
    ) = new &&[L2, V2](argPlan, maxDepth)

    case class &&[L2 <: Local.AnyGraph._Axiom, V2](
        argPlan: LocalEngine.PlanK.Compat[L2, V2],
        override val maxDepth: Int = ^.this.maxDepth
    ) extends AnyGraphBinary {

      override type Prev = ^.this.type
      override val prev: ^.this.type = ^.this

      override type ArgLaw = L2

      override type ArgV = V2
    }
  }

  // TODO: memoize all views to avoid duplicated computation
  //  def make[II <: Plan.Lt[Local.Graph[_]]](
  //      override val inputPlan: II
  //  ): ^[arg.OV, TT]
}
