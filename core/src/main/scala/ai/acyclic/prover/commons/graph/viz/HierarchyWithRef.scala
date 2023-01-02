package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Graph.Outbound
import ai.acyclic.prover.commons.graph.Graph.Outbound.ArrowUBK
import ai.acyclic.prover.commons.graph.processing.GraphPlans
import ai.acyclic.prover.commons.graph.{Graph, Tree}

import java.util.UUID
import scala.collection.mutable

object HierarchyWithRef extends Visualisations {

  val graphSys: Graph.Outbound.type = Graph.Outbound

  implicit lazy val default: Canonical = Canonical(Hierarchy.default)

  trait BuiltIn extends HierarchyWithRef {}

  case class Canonical(
      base: Hierarchy
  ) extends BuiltIn {

    override lazy val traversal: GraphPlans.TraversePlan = {
      GraphPlans.Traverse(down = { v => v.resolve() }).DepthFirst
    }

    override def sameRefBy(node: Outbound.Node): Outbound.Node = node
  }
}

trait HierarchyWithRef extends HierarchyWithRef.Format {

  def base: Hierarchy

  def bindings: LazyList[String] = (1 to Int.MaxValue).to(LazyList).map(v => "" + v)

  def traversal: GraphPlans.TraversePlan

  def sameRefBy(node: Graph.Outbound.Node): Any

  trait Viz[N <: HierarchyWithRef.graphSys.Node] extends HierarchyWithRef.TextViz[N]

  // shared between visualisations of multiple graphs
  case class Group() {
    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    case class Reference(id: UUID = UUID.randomUUID())(
        node: Graph.Outbound.Node
    ) extends Tree.Node {

      lazy val refKey: Any = sameRefBy(node)

      lazy val (sameRefs: SameRefs, shouldExpand: Boolean) = {

        var shouldExpand = false
        val sameRefs = expanded
          .get(refKey)
          .map { existing =>
            val nextBinding = bindings(binded.size)
            binded.put(refKey, nextBinding)

            existing.buffer += this
            existing
          }
          .getOrElse {
            shouldExpand = true

            val result = SameRefs()
            result.buffer += this

            expanded.put(refKey, result)
            result
          }

        sameRefs -> shouldExpand
      }

      lazy val bindingOpt: Option[String] = binded.get(refKey)

      override def resolve(): Unit = {
        sameRefs
        super.resolve()
      }

      override lazy val outbound: Seq[ArrowUBK[Tree.Node]] = {
        if (!shouldExpand) Nil
        else {
          node.outbound.toSeq.map { arrow =>
            val target: Reference = Reference()(arrow.target)
            arrow.arrowType.NoInfo[Reference](target) // this discard info from arrows
          }
        }
      }

      override lazy val nodeText: String = {
        val bindingText = bindingOpt
          .map { v =>
            s" ...... [$v]"
          }
          .getOrElse("")

        node.nodeText + bindingText
      }
    }

    case class SameRefs() {

      val buffer: mutable.ArrayBuffer[Reference] = mutable.ArrayBuffer.empty
    }

    case class Viz[N <: Graph.Outbound.Node](node: N) extends HierarchyWithRef.this.Viz[N] {

      override val outer: HierarchyWithRef.this.type = HierarchyWithRef.this

      lazy val delegate = {

        val reference = Reference()(node)
        traversal.exeOn(reference)
        reference.showHierarchy(base)
      }

      override def treeString: String = {

        delegate.treeString
      }
    }
  }
}
