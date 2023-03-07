package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.EqualBy
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.GraphSystem._Graph
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.{GraphExpr, PlanGroup}
import shapeless.Sized

import scala.collection.mutable
import scala.language.existentials

case class GraphUnary[IG <: _Graph, N](arg: GraphExpr[IG])(
    implicit
    ev: IG <:< Graph[N]
    // see https://stackoverflow.com/questions/16291313/scala-inferred-type-arguments-type-bounds-inferring-to-nothing
) extends PlanGroup.Unary.Expressions[IG] {

  final override lazy val args = Sized(arg)

  lazy val input: IG = arg.resolve

  trait TraverseLike extends Expr[IG] {}

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: N => Unit = { _: N => {} },
      up: N => Unit = { _: N => {} }
  ) {

    object DepthFirst extends TraverseLike {

      override def exe: IG = {

        def exeInternal(node: N, maxDepth: Int): Unit = {

          down(node)

          val arrows: Seq[Arrow.Of[N]] =
            if (maxDepth == 0) Nil
            else input.nodeOps(node).induction

          arrows.foreach { arrow =>
            val target = arrow.target
            exeInternal(target, maxDepth - 1)
          // TODO: may incur high overhead
          }

          up(node)
        }

        input.roots.foreach { ii =>
          exeInternal(ii, maxDepth)
        }

        input
      }
    }

    object DepthFirst_ForEach extends TraverseLike {

      private val _down = down
      private val _up = up

      override def exe: IG = {

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

  case class Upcast[N2 >: N]() extends Expr[Graph[N2]] {

    object Upcasted extends Graph[N2] {

      case class Ops(node: N2) extends GraphNOps {

        override protected def getInduction: Seq[Arrow.Of[N2]] = {
          node match {
            case v: N => input.nodeOps(v).induction
            case _    => ???
          }
        }
      }

      override def roots: Rows[N2] = input.roots.map(v => v: N2)
    }

    override def exe: Graph[N2] = {

      Upcasted
    }
  }

//  case class Rewritten[G1 <: Graph[Int], G2 <: Graph[String]](original: G1) {
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
//
//  case class Rewrite[G2 <: Graph[N]](
//      rewriter: Transcriber[N, G2]
//  ) {
//
//    // how to cross GraphType & NodeType?
//    // if this is the optimising phase of a compiler: how to ensure that there is no information loss?
//    // how to ensure that graph AFTER transformation can be traced back to BEFORE transformation?
//    // all important questions ...
//    case class Transform(
//        maxDepth: Int = Int.MaxValue,
//        down: N => N = identity[N],
//        up: N => N = identity[N]
//    ) {
//
//      object DepthFirst extends _Transform[N] {
//
//        override def exe: G2 = {
//
//          val g2BeforeT = rewriter.build(input.roots)
//
//          val newRoots: Seq[N] = g2BeforeT.roots.map { node =>
//            val downT = down(node)
//
//            input.nodeOps(downT)
//          }
//
//          def subExeOn(node: N, maxDepth: Int): Unit = {
//
//            ???
//          }
//
//          ???
//        }
//      }
//    }
//  }
}
