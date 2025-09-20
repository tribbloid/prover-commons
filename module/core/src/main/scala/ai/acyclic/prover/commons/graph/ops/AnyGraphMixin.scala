package ai.acyclic.prover.commons.graph.ops

import ai.acyclic.prover.commons.graph.topology.{Axiom, Topology}
import ai.acyclic.prover.commons.graph.{Engine, Foundation}
import ai.acyclic.prover.commons.multiverse.CanEqual

import scala.language.implicitConversions

trait AnyGraphMixin {
  self: Engine =>

  case class AnyGraphOps1[
      X <: Topology.AnyGraph._Axiom,
      V
  ](
      override val arg: Graph[X, V]
  ) extends Ops.Unary[X, V](arg) {

    {
      implicitly[arg.axiom.type <:< AnyGraph._Axiom]
    }

    import AnyGraphOps1.*

    //  lazy val asLazyList: LazyList[inputG.Value] = asIterable.to(LazyList)

    case class NodeMap[V2](
        fn: V => V2
    ) extends Plan[V2] {

      override def entries: Batch[Node[X, V2]] = {

        val known = arg.distinctEntries

        val newNode = known.map { n =>
          n.map(v => fn(v): V2)
        }

        newNode
      }
    }

    object NodeUpcast { // TODO: remove, All Graphs are already [+V]

      def apply[V2 >: V]: NodeMap[V2] = NodeMap[V2](v => v: V2)
    }

    // TODO:
    //  need to transcribe to a more constraining graph type
    case class Transform(
        setter: ArgSetter,
        pruning: Pruning[ArgNode] = identity,
        down: ArgNode => Seq[ArgNode] = v => Seq(v),
        up: ArgNode => Seq[ArgNode] = v => Seq(v)
    ) {

      trait TransformPlan extends Plan[V]

      lazy val _setter: ArgSetter = setter.Verified

      object DepthFirst extends TransformPlan {

        private def transformInternal(node: ArgNode, depth: Int = arg.maxRecursionDepth): Seq[ArgNode] = {
          if (depth > 0) {

            def doTransform(n: ArgNode): Seq[ArgNode] = {
              val downNs: Seq[ArgNode] = down(n)

              val inductionTs: Seq[ArgNode] =
                downNs.map { n =>
                  val successors = n.inductionNodes
                  val successorsTransformed = successors.flatMap { nn =>
                    transformInternal(nn, depth - 1)
                  }
                  val rewritten = _setter.update(n)(successorsTransformed)
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

        override def entries: Batch[ArgNode] = {

          val transformed = arg.distinctEntries.flatMap(n => transformInternal(n, maxRecursionDepth))

          transformed
        }
      }

      object DepthFirst_Once extends TransformPlan {

        private val delegate = {

          val evaled = CanEqual.Native.Lookup[Any, ArgNode]()

          Transform(
            setter,
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

        override def entries: Batch[ArgNode] = {

          delegate.entries
        }
      }

      object DepthFirst_Cached extends TransformPlan {

        private val delegate = {

          val evaled = CanEqual.Native.Lookup[Any, Seq[ArgNode]]()

          Transform(
            setter,
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

        override def entries: Batch[ArgNode] = {
          delegate.entries
        }
      }
    }

    object TransformLinear {
      def apply(
          rewriter: ArgSetter,
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

    // NOT ForeachNode! Traversal may visit a node multiple times.
    case class Traverse(
        down: ArgNode => Unit = { (_: ArgNode) => {} },
        up: ArgNode => Unit = { (_: ArgNode) => {} }
    ) {

      private val delegate = Transform(
        setter = Foundation.Updater.DoNotRewrite(arg.axiom),
        down = { v =>
          down(v); Seq(v)
        },
        up = { v =>
          up(v); Seq(v)
        }
      )

      trait TraversePlan extends Plan[V] {
        override val entries: Batch[ArgNode] // must evaluate eagerly
      }

      object DepthFirst extends TraversePlan {

        override val entries: Batch[ArgNode] = {

          delegate.DepthFirst.entries
        }
      }

      object DepthFirst_Once extends TraversePlan {

        override val entries: Batch[ArgNode] = {

          delegate.DepthFirst_Once.entries
        }
      }
    }

    def ><[
        X2 >: X <: Axiom.Top,
        V2 >: V
    ](arg2: Graph[X2, V2]): AnyGraphOps2[X2, V2] = {

//      val upcasted = this.copy[X2, V2]()

      AnyGraphOps2[X2, V2](this, arg2)
    }

    def ops_anyGraph: AnyGraphOps1[X, V] = this
  }

  object AnyGraphOps1 {

    type Pruning[N] = (N => Seq[N]) => (N => Seq[N])

    implicit def upcast[
        X <: Topology.AnyGraph._Axiom,
        V
    ](self: AnyGraphOps1[? <: X, ? <: V]): AnyGraphOps1[X, V] = {
      self.copy[X, V]()
    }
  }

  implicit def imp_anyGraph[
      X <: Topology.AnyGraph._Axiom,
      V
  ](
      arg: Graph[X, V]
  ): AnyGraphOps1[X, V] = AnyGraphOps1[X, V](arg)

  case class AnyGraphOps2[
      X <: Axiom.Top,
      VV
  ](
      override val prev: AnyGraphOps1[X, VV],
      override val arg: Graph[X, VV]
  ) extends Ops.Binary[X, VV](arg) {

    // union by node identity
    // always an anygraph, union of 2 semilattices may contain cycles
    case class Union() extends AnyGraph.Plan[VV] {

      override lazy val entries: Batch[AnyGraph.Node[VV]] = {

        val e1: Batch[prev.ArgNode] = prev.arg.distinctEntries
        val e2: Batch[ArgNode] = arg.distinctEntries

        // the axiom bound can be tighten further
        //        val roots1: Seq[Foundation.NodeK.Lt[Axiom.AnyGraphT, VV]] = e1.map(n => n.upcast[VV])
        //        val roots2: Seq[Foundation.NodeK.Lt[Axiom.AnyGraphT, VV]] = e2.map(n => n.upcast[VV])

        val result = (e1 union e2).distinct
        result
      }
    }
  }
}
