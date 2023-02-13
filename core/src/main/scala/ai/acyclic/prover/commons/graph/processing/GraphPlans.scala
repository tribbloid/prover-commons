package ai.acyclic.prover.commons.graph.processing

import ai.acyclic.prover.commons.EqualBy
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Graph

import scala.collection.mutable

import scala.language.existentials

case class GraphPlans[N](g: Graph[N]) {

  import GraphPlans._

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: N => Unit = { _: N => {} },
      up: N => Unit = { _: N => {} }
      //      sortFn: Seq[N] => Seq[N] = identity[Seq[N]] _
  ) {

    object DepthFirst extends _Traverse[N] {

      override def exe: OUB[N] = {

        def exeInternal(node: N, maxDepth: Int): Unit = {

          down(node)

          val arrows: Seq[Arrow.Of[N]] =
            if (maxDepth == 0) Nil
            else g.nodeOps(node).induction

          arrows.foreach { arrow =>
            val target = arrow.target
            exeInternal(target, maxDepth - 1)
          // TODO: may incur high overhead
          }

          up(node)
        }

        g.roots.foreach { ii =>
          exeInternal(ii, maxDepth)
        }

        g
      }
    }

    object DepthFirst_ForEach extends _Traverse[N] {

      private val _down = down
      private val _up = up

      override def exe: Graph[N] = {

        val visitedDown = mutable.Map.empty[EqualBy.MemoryHash[N], N]
        val visitedUp = mutable.Map.empty[EqualBy.MemoryHash[N], N]

        lazy val delegate = {
          val tt = Traverse(
            maxDepth,
            down = { node =>
              visitedDown.getOrElseUpdate(
                EqualBy.MemoryHash(node), {
                  _down(node)
                  node
                }
              )
            },
            up = { node =>
              visitedUp.getOrElseUpdate(
                EqualBy.MemoryHash(node), {
                  _up(node)
                  node
                }
              )
            }
          )
          tt.DepthFirst
        }

        delegate.exe
      }
    }
  }

  case class Transform[N2](
      maxDepth: Int = Int.MaxValue,
      down: N => Unit = { _: N => {} },
      up: N => Unit = { _: N => {} }
      //      sortFn: Seq[N] => Seq[N] = identity[Seq[N]] _
  ) {

    object DepthFirst extends _Traverse[N] {

      override def exe: OUB[N] = {

        def subExeOn(node: N, maxDepth: Int): Unit = {

          down(node)

          g.nodeOps(node).induction.foreach { arrow =>
            val target = arrow.target
            subExeOn(target, maxDepth - 1)
          // TODO: may incur high overhead
          }

          up(node)
        }

        g.roots.foreach { ii =>
          subExeOn(ii, maxDepth)
        }

        g
      }
    }
  }

}

object GraphPlans extends PlanType {

  type OUB[N] = Graph[N]

  trait _Traverse[N] extends Unary[N] {}
}
