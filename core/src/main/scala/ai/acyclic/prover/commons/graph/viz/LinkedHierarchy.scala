package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Topology.TreeT
import ai.acyclic.prover.commons.graph.local.{Graph, Local, Tree}
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import ai.acyclic.prover.commons.typesetting.TextBlock

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LinkedHierarchy extends Visualisations {

  def addSrcAnnotation(body: String, binding: String): String = {

    if (body.isEmpty) return body

    val block = TextBlock(body)

    val ll = block.lines.head.length
    val ellipseLL = 80 - ll
    val annotations = " " + String.valueOf((1 to ellipseLL).map(_ => '.').toArray) + s" [$binding]"

    block
      .appendPerLine(TextBlock(annotations))
      .build
  }

  def addRefAnnotation(body: String, binding: String): String = {

    if (body.isEmpty) return body

    val block = TextBlock(body)

    val ellipseLL = 3
    val annotations = " " + String.valueOf((1 to ellipseLL).map(_ => '.').toArray) + s" (see [$binding])"

    block
      .appendPerLine(TextBlock(annotations))
      .build
  }

  type UB[N] = Graph.Outbound[N]

  implicit lazy val defaultFormat: Default = new Default(Hierarchy.default)

  implicit def newGroup: defaultFormat.Group = defaultFormat.Group()

  class Default(
      val backbone: Hierarchy
  ) extends LinkedHierarchy {

    override def sameRefBy(node: Graph.Outbound.Node[_]): Option[Any] = Some(node)

    override def dryRun(tree: Tree[_ <: _RefBinding]): Unit = {
      GraphUnary
        .make(tree)
        .Traverse(
          maxDepth = backbone.maxDepth,
          down = { n =>
            n.induction
          }
        )
        .DepthFirst
        .compute
    }
  }

  trait _RefBinding {

    def node: Any
  }

  type SameRefs = mutable.ArrayBuffer[_RefBinding]
  object SameRefs {
    def apply(): SameRefs = mutable.ArrayBuffer.empty
  }

}

trait LinkedHierarchy extends LinkedHierarchy.Format {

  import LinkedHierarchy._

  def backbone: Hierarchy

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def dryRun(tree: Tree[_ <: _RefBinding]): Unit

  def sameRefBy(node: Graph.Outbound.Node[_]): Option[Any]

  trait _Viz[V] extends TextViz[V]

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    lazy val bindingIndices = new AtomicInteger(0)

    case class Viz[V](override val graph: UB[V]) extends _Viz[V] {

      object RefBindingT extends TreeT.Untyped {

        case class Node(
                         node: Graph.Outbound.Node[V],
                         id: UUID = UUID.randomUUID()
        ) extends UntypedNode
            with _RefBinding {

          {
            sameRefs_shouldExpand
          }

          lazy val refKeyOpt: Option[Any] = sameRefBy(node)

          lazy val sameRefs_shouldExpand = {

            val existing = refKeyOpt
              .map { refKey =>
                val result = expanded
                  .get(refKey)
                  .map { sameRefs =>
                    binded.getOrElseUpdate(
                      refKey,
                      bindings(bindingIndices.getAndIncrement())
                    )

                    sameRefs += this
                    sameRefs -> false
                  }
                  .getOrElse {

                    val result = SameRefs()
                    result += this

                    expanded.put(refKey, result)
                    result -> true
                  }
                result
              }

            val result = existing
              .getOrElse {
                SameRefs() -> true
              }

            result
          }

          def sameRefs: SameRefs = sameRefs_shouldExpand._1

          def shouldExpand: Boolean = sameRefs_shouldExpand._2

          lazy val bindingNameOpt: Option[String] = {

            refKeyOpt.flatMap { k =>
              binded.get(k)
            }
          }

          override protected def inductionC = {

            if (!shouldExpand) {
              Nil
            } else {

              val result = node.induction.map { v =>
                v._1 -> Node(v._2) // this discard arrow info
              }

              result
            }
          }

          override protected def nodeTextC: String = {

            val originalText = node.nodeText

            bindingNameOpt
              .map { name =>
                if (shouldExpand) addSrcAnnotation(originalText, name)
                else addRefAnnotation(originalText, name)
              }
              .getOrElse(originalText)
          }
        }
      }

      lazy val delegates: Seq[Tree[RefBindingT.Node]] = {
        val roots: Local.Dataset[Graph.Outbound.Node[V]] = graph.entriesC
        roots.map { node =>
          val refBinding: RefBindingT.Node = RefBindingT.Node(node)
          Tree(refBinding)
        }
      }

      def dryRun(): Unit = {

        delegates.foreach { g =>
          LinkedHierarchy.this.dryRun(g)
        }
      }

      override lazy val treeString: String = {
        dryRun()

        delegates
          .map { v =>
            backbone.Viz(v).treeString
          }
          .mkString("\n")
      }
    }
  }
}
