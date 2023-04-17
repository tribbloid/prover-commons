package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Topology.TreeT
import ai.acyclic.prover.commons.graph.local.{Graph, Tree}
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import ai.acyclic.prover.commons.graph.Structure
import ai.acyclic.prover.commons.viz.text.TextBlock

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

    override def sameRefBy(node: Any): Option[Any] = Some(node)

    override def dryRun[N <: _RefBinding](tree: Tree[N]): Unit = {
      GraphUnary
        .make(tree)
        .Traverse(
          maxDepth = backbone.maxDepth,
          down = { v =>
            val ops = tree.ops(v)
            ops.valueInduction
          }
        )
        .DepthFirst
        .exe
    }
  }

  trait _RefBinding {

    def node: Any
  }

  case class SameRefs() {

    val buffer: mutable.ArrayBuffer[_RefBinding] = mutable.ArrayBuffer.empty
  }
}

trait LinkedHierarchy extends LinkedHierarchy.Format {

  import LinkedHierarchy._

  def backbone: Hierarchy

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def dryRun[N <: _RefBinding](tree: Tree[N]): Unit

  def sameRefBy(node: Any): Option[Any]

  trait _Viz[N] extends TextViz[N]

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    lazy val bindingIndices = new AtomicInteger(0)

    case class Viz[N](override val graph: UB[N]) extends _Viz[N] {

      case class RefBinding(node: N, id: UUID = UUID.randomUUID()) extends _RefBinding {

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

                  sameRefs.buffer += this
                  sameRefs -> false
                }
                .getOrElse {

                  val result = SameRefs()
                  result.buffer += this

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
        def sameRefs = sameRefs_shouldExpand._1
        def shouldExpand = sameRefs_shouldExpand._2

        lazy val bindingOpt: Option[String] = {

          refKeyOpt.flatMap { k =>
            binded.get(k)
          }
        }

      }

      case class RefTree(node: N) extends Tree[RefBinding] with Structure.Immutable[TreeT._Node[RefBinding]] {

        override lazy val root: RefBinding = RefBinding(node)

        case class Ops(override val value: RefBinding) extends TreeT._Node[RefBinding] {

          def originalOps = graph.ops(value.node)

          override protected def getInduction = {

            if (!value.shouldExpand) {
              Nil
            } else {

              val result = originalOps.valueInduction.map { v =>
                v._1 -> Ops(RefBinding(v._2)) // this discard arrow info
              }

              result
            }
          }

          override protected def getNodeText: String = {

            val originalText = originalOps.nodeText

            value.bindingOpt
              .map { binding =>
                if (value.shouldExpand) addSrcAnnotation(originalText, binding)
                else addRefAnnotation(originalText, binding)
              }
              .getOrElse(originalText)
          }
        }
      }

      lazy val delegates: Seq[RefTree] = {
        graph.roots
          .map { node =>
            RefTree(node)
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
