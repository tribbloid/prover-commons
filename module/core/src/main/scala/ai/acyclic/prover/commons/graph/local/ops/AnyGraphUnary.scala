package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.*
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Flow
import ai.acyclic.prover.commons.multiverse.CanEqual

trait AnyGraphUnary extends Local.AnyGraph.Ops.Unary {

//  {
//    implicitly[ArgLaw <:< Local.AnyGraph._Axiom]
//  }

  import AnyGraphUnary.*

  def isEmpty: Boolean = arg.entries.isEmpty

  lazy val distinctEntries: Vector[arg.NodeV] = arg.entries.distinct

  def text_flow(
      implicit
      format: Flow
  ): format.Viz[arg.Value] = format.Viz(arg)

  // TODO: move into `object collect`
  object asIterable extends Iterable[arg.Value] {

    override def iterator: Iterator[arg.Value] = {
      val base: Iterator[arg.NodeV] = distinctEntries.iterator

      val result = base.flatMap { bb: arg.NodeV =>
        val result = bb.asIterable.map(v => v.asInstanceOf[arg.Value])
        result
      }

      result
    }
  }

//  lazy val asLazyList: LazyList[inputG.Value] = asIterable.to(LazyList)

  case class NodeMap[V2](
      fn: arg.Value => V2
  ) extends LocalEngine._PlanK[arg._Axiom] {

    type Value = V2

    override lazy val entries: Vector[NodeV] = {

      val newNode: Vector[NodeV] = distinctEntries.map { (n: arg.NodeV) =>
        n.asInstanceOf[Node[arg.Value]].map { (v: arg.Value) =>
          fn(v).asInstanceOf[Value]
        }
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
      rewriter: arg.RewriterV,
      down: arg.NodeV => Seq[arg.NodeV] = v => Seq(v),
      up: arg.NodeV => Seq[arg.NodeV] = v => Seq(v),
      pruning: Pruning[arg.NodeV] = identity
  ) {

    lazy val rewriterVerified: arg.RewriterV = rewriter.Verified

    trait _Plan extends LocalEngine._GraphK[arg._Axiom] {

      type _Axiom = arg._Axiom
      type Value = arg.Value
    }

    object DepthFirst extends _Plan {

      private def transformInternal(
          node: arg.NodeV,
          depth: Int
      ): Seq[NodeV] = {
        if (depth > 0) {

          def doTransform(n: arg.NodeV): Seq[arg.NodeV] = {
            val downNs: Seq[arg.NodeV] = down(n)

            val inductionTs: Seq[arg.NodeV] =
              downNs.map { n =>
                val successors = n.discoverNodes
                val successorsTransformed: Seq[arg.NodeV] = successors.flatMap { nn =>
                  transformInternal(nn, depth - 1).asInstanceOf[Seq[arg.NodeV]]
                }
                val rewritten = rewriterVerified.rewrite(n)(successorsTransformed)
                rewritten
              }

            val results = inductionTs.flatMap { n =>
              up(n)
            }

            results
          }

          val result: Seq[NodeV] = pruning(doTransform).apply(node).asInstanceOf[Seq[NodeV]]
          result
        } else {
          Seq(node.asInstanceOf[NodeV])
        }
      }

      override lazy val entries: Vector[NodeV] = {

        val transformed: Vector[NodeV] = distinctEntries.flatMap(n => transformInternal(n, maxDepth))
        transformed
      }
    }

    object DepthFirst_Once extends _Plan {

      private val delegate = {

        val evaled = CanEqual.Native.Lookup[Any, arg.NodeV]()

        Transform(
          rewriter,
          down,
          up,
          pruning = { fn =>
            { node =>
              val keyOpt = node.evalCacheKey

              val result: Seq[arg.NodeV] = keyOpt.flatMap { key =>
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
          }
        ).DepthFirst
      }

      override lazy val entries: Vector[NodeV] = {

        delegate.entries
      }

    }

    object DepthFirst_Cached extends _Plan {

      private val delegate = {

        val evaled = CanEqual.Native.Lookup[Any, Seq[arg.NodeV]]()

        Transform(
          rewriter,
          down,
          up,
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
          }
        ).DepthFirst
      }

      override lazy val entries: Vector[NodeV] = {

        delegate.entries
      }
    }
  }

  object TransformLinear {
    def apply(
        rewriter: arg.RewriterV,
        down: arg.NodeV => arg.NodeV = v => v,
        pruning: Pruning[arg.NodeV] = identity,
        up: arg.NodeV => arg.NodeV = v => v
    ): Transform = Transform(
      rewriter,
      v => Seq(down(v)),
      v => Seq(up(v)),
      pruning
    )
  }

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      down: arg.NodeV => Unit = { (_: arg.NodeV) => {} },
      up: arg.NodeV => Unit = { (_: arg.NodeV) => {} }
  ) {

    @transient lazy val delegate: Transform = Transform(
      rewriter = RewriterK.DoNotRewrite(),
      down = { v =>
        down(v); Seq(v)
      },
      up = { v =>
        up(v); Seq(v)
      }
    )

    lazy val DepthFirst = delegate.DepthFirst

    lazy val DepthFirst_Once = delegate.DepthFirst_Once
  }
}

object AnyGraphUnary {

  type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

  case class ^[A <: Local.AnyGraph.Graph[?]](
      arg: A,
      override val maxDepth: Int = 20
  ) extends AnyGraphUnary {

    type Arg = A

    def &&[B <: Local.AnyGraph.Graph[?]](
        arg: B,
        maxDepth: Int = ^.this.maxDepth
    ) = new &&(arg, maxDepth)

    case class &&[B <: Local.AnyGraph.Graph[?]](
        arg: B,
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
