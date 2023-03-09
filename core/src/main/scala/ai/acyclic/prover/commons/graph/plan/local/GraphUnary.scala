package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.EqualBy
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.GraphSystem._Graph
import ai.acyclic.prover.commons.graph.local.{Graph, Rewriter}
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

  lazy val inputGraph: IG = arg.exeOnce

  case class UpcastNode[N2 >: N]() extends Expr[Graph[N2]] {

    object Upcasted extends Graph[N2] {

      case class Ops(node: N2) extends GraphNOps {

        override protected def getNodeText: String = inputGraph.nodeOps(node.asInstanceOf[N]).nodeText

        override protected def getInduction: Seq[Arrow.Of[N2]] = {
          inputGraph.nodeOps(node.asInstanceOf[N]).induction
        }
      }

      override def roots: Rows[N2] = inputGraph.roots.map(v => v: N2)
    }

    override def exe: Graph[N2] = {

      Upcasted
    }
  }

  trait TransformLike extends Expr[Graph[N]]

  // TODO:
  //  need to cross NodeType
  //  need to transcribe to a different graph type
  case class Transform(
      rewriter: Rewriter[N],
      maxDepth: Int = Int.MaxValue,
      down: N => Seq[N] = v => Seq(v),
      pruning: N => Boolean = _ => true,
      up: N => Seq[N] = v => Seq(v)
  ) {

    trait LazyResultGraph extends Graph[N] {

      private def transformInternal(node: N, depth: Int = maxDepth): Seq[N] = {

        if (pruning(node) && depth > 0) {
          val downTs: Seq[N] = down(node)

          val inductionTs: Seq[N] =
            downTs.map { n =>
              val successors = inputGraph.nodeOps(n).successors
              val successorsTransformed = successors.flatMap { nn =>
                transformInternal(nn, depth - 1)
              }
              val rewritten = rewriter.VerifiedOn(inputGraph).apply(n)(successorsTransformed)
              rewritten
            }

          val results = inductionTs.flatMap { n =>
            up(n)
          }

          results
        } else {
          Seq(node)
        }
      }

      override lazy val roots: Seq[N] = {
        inputGraph.roots.flatMap(n => transformInternal(n))
      }

      case class Ops(node: N) extends GraphNOps {
        override protected def getNodeText: String = inputGraph.nodeOps(node).nodeText

        override protected def getInduction: Seq[Arrow.Of[N]] = inputGraph.nodeOps(node).induction
      }
    }
    object LazyResultGraph extends LazyResultGraph // TODO: this should be exposed

    object ResultGraph extends LazyResultGraph {

      {
        roots
      }
    }

    object DepthFirst extends TransformLike {

      override def exe: Graph[N] = {
        ResultGraph
      }
    }

    object DepthFirst_Once extends TransformLike {

      private val delegate = {

        val seen = mutable.Map.empty[EqualBy.MemoryHash[N], N]

        Transform(
          rewriter,
          maxDepth,
          down,
          { node =>
            var isPruned = false
            seen.getOrElseUpdate(
              EqualBy.MemoryHash(node), {
                isPruned = true
                node
              }
            )
            isPruned
          },
          up
        )
      }

      override def exe: Graph[N] = {
        delegate.DepthFirst.exe
      }
    }
  }

  object TransformLinear {
    def apply(
        rewriter: Rewriter[N],
        maxDepth: Int = Int.MaxValue,
        down: N => N = v => v,
        pruning: N => Boolean = _ => true,
        up: N => N = v => v
    ): Transform = Transform(
      rewriter,
      maxDepth,
      v => Seq(down(v)),
      pruning,
      v => Seq(up(v))
    )
  }

//  {
//
//    val delegate: Transform = Transform(
//      rewriter,
//      maxDepth,
//      v => Seq(down(v)),
//      pruning,
//      v => Seq(up(v))
//    )
//  }
//
//  object TransformLinear {
//
//    implicit def asTransform(v: TransformLinear): Transform = v.delegate
//  }

  trait TraverseLike extends Expr[IG] {}

  // NOT ForeachNode! Traversal may visit a node multiple times.
  case class Traverse(
      maxDepth: Int = Int.MaxValue,
      down: N => Unit = { _: N => {} },
      up: N => Unit = { _: N => {} }
  ) {

    private val delegate = Transform(
      rewriter = Rewriter.DoNotRewrite[N](),
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

    object DepthFirst_ForEach extends TraverseLike {

//      private val _down = down
//      private val _up = up

      override def exe: IG = {

        delegate.DepthFirst_Once.exe
        inputGraph
      }
    }
  }

  // TODO: delete the following example, note sure if map or monadic flatMap can be supported
  //  case class TransformNode[G1 <: Graph[Int], G2 <: Graph[String]](original: G1) {
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

}
