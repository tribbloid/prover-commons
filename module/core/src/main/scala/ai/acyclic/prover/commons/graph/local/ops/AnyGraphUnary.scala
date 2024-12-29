package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.Refinement
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.topology.Axiom
import ai.acyclic.prover.commons.graph.viz.Flow
import ai.acyclic.prover.commons.multiverse.CanEqual

trait AnyGraphUnary extends Local.AnyGraph.Ops.Unary {

  {
    implicitly[arg.axiom.type <:< Local.AnyGraph._Axiom]
  }

  import AnyGraphUnary.*

  def isEmpty: Boolean = arg.entries.isEmpty

  lazy val distinctEntries: Vector[ArgNode] = arg.entries.distinct

  def text_flow(
      implicit
      format: Flow
  ): format.Viz[arg.Value] = format.Viz(arg)

  object asIterable extends Iterable[arg.Value] {

    override def iterator: Iterator[arg.Value] = {
      val base = distinctEntries.iterator

      base.flatMap { bb =>
        bb.asIterable.iterator
      }
    }
  }

//  lazy val asLazyList: LazyList[inputG.Value] = asIterable.to(LazyList)

  case class NodeMap[V2](
      fn: arg.Value => V2
  ) extends Plan__[V2] {

    override def getEntries: Vector[Refinement.NodeK.Lt[arg._Axiom, V2]] = {

      val known: Vector[ArgNode] = distinctEntries

      val newNode = known.map { n =>
        n.map(v => fn(v): V2)
      }

      newNode
    }
  }

  object NodeUpcast {

    def apply[V2 >: arg.Value]: NodeMap[V2] = NodeMap[V2](v => v: V2)
  }

  // TODO:
  //  need to transcribe to a more constraining graph type
  case class Transform(
      rewriter: ArgRewriter,
      pruning: Pruning[ArgNode] = identity,
      down: ArgNode => Seq[ArgNode] = v => Seq(v),
      up: ArgNode => Seq[ArgNode] = v => Seq(v)
  ) {

    trait TransformPlan extends Plan__[arg.Value]

    lazy val _rewriter: ArgRewriter = rewriter.Verified

    object DepthFirst extends TransformPlan {

      private def transformInternal(node: ArgNode, depth: Int = maxDepth): Seq[ArgNode] = {
        if (depth > 0) {

          def doTransform(n: ArgNode): Seq[ArgNode] = {
            val downNs: Seq[ArgNode] = down(n)

            val inductionTs: Seq[ArgNode] =
              downNs.map { n =>
                val successors = n.adjacentNodes
                val successorsTransformed = successors.flatMap { nn =>
                  transformInternal(nn, depth - 1)
                }
                val rewritten = _rewriter.rewrite(n)(successorsTransformed)
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

      override def getEntries: Vector[ArgNode] = {

        val transformed = distinctEntries.flatMap(n => transformInternal(n, maxDepth))

        transformed
      }
    }

    object DepthFirst_Once extends TransformPlan {

      private val delegate = {

        val evaled = CanEqual.Native.Lookup[Any, ArgNode]()

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

      override def getEntries: Vector[ArgNode] = {

        delegate.getEntries
      }
    }

    object DepthFirst_Cached extends TransformPlan {

      private val delegate = {

        val evaled = CanEqual.Native.Lookup[Any, Seq[ArgNode]]()

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

      override def getEntries: Vector[ArgNode] = {
        delegate.getEntries
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

  // TODO: the following should go through Transform, need test to ensure working
  trait TraversePlan extends Plan__[arg.Value]

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      down: ArgNode => Unit = { (_: ArgNode) => {} },
      up: ArgNode => Unit = { (_: ArgNode) => {} }
  ) {

    private val delegate = Transform(
      rewriter = Refinement.RewriterK.DoNotRewrite(arg.axiom),
      down = { v =>
        down(v); Seq(v)
      },
      up = { v =>
        up(v); Seq(v)
      }
    )

    object DepthFirst extends TraversePlan {

      override def getEntries: Vector[ArgNode] = {

        delegate.DepthFirst.getEntries
      }
    }

    object DepthFirst_Once extends TraversePlan {

      override def getEntries: Vector[ArgNode] = {

        delegate.DepthFirst_Once.getEntries
      }
    }
  }

}

object AnyGraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  case class ^[A <: Local.AnyGraph[?]](
      override val arg: A,
      override val maxDepth: Int = 20
  ) extends AnyGraphUnary {

    type Arg = A

    def ><[B <: Local.AnyGraph[?]](
        argPlan: B,
        maxDepth: Int = ^.this.maxDepth
    ) = new _Binary(argPlan, maxDepth)

    case class _Binary[B <: Local.AnyGraph[?]](
        override val arg: B,
        override val maxDepth: Int = ^.this.maxDepth
    ) extends AnyGraphBinary {

      type Arg = B

      override type Prev = ^.this.type
      override val prev: ^.this.type = ^.this
    }
  }

  // TODO: memoize all views to avoid duplicated computation
  //  def make[II <: Plan.Lt[Local.Graph[_]]](
  //      override val inputPlan: II
  //  ): ^[arg.OV, TT]
}
