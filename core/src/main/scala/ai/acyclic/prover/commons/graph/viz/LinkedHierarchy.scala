package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.{Graph, Tree}
import ai.acyclic.prover.commons.graph.processing.GraphPlans
import ai.acyclic.prover.commons.viz.text.TextBlock

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LinkedHierarchy extends Visualisations {

  def addAnnotation(body: String, binding: String): String = {

    if (body.isEmpty) return body

    val block = TextBlock(body)

    val ll = block.lines.head.length
    val ellipseLL = 80 - ll
    val annotations = " " + String.valueOf((1 to ellipseLL).map(_ => '.').toArray) + s" [$binding]"

    block
      .appendPerLine(TextBlock(annotations))
      .build
  }

  type UB[N] = Graph.Outbound[N]

  implicit lazy val defaultFormat: Canonical = new Canonical(Hierarchy.default)

  implicit def newGroup: defaultFormat.Group = defaultFormat.Group()

  trait BuiltIn extends LinkedHierarchy {}

  class Canonical(
      val backbone: Hierarchy
  ) extends BuiltIn {

    override def sameRefBy(node: Any): Option[Any] = Some(node)

    override def dryRun[N <: _RefNode](tree: Tree[N]): Unit = {
      GraphPlans(tree)
        .Traverse(
          down = { v =>
            val ops = tree.nodeOps(v)
            ops.induction
          }
        )
        .DepthFirst
        .exe
    }
  }

  trait _RefNode {

    def node: Any
  }

  case class SameRefs() {

    val buffer: mutable.ArrayBuffer[_RefNode] = mutable.ArrayBuffer.empty
  }
}

trait LinkedHierarchy extends LinkedHierarchy.Format {

  import LinkedHierarchy._

  def backbone: Hierarchy

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def dryRun[N <: _RefNode](tree: Tree[N]): Unit

  def sameRefBy(node: Any): Option[Any]

  trait _Viz[N] extends TextViz[N]

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    lazy val bindingIndices = new AtomicInteger(0)

    case class Viz[N](override val graph: UB[N]) extends _Viz[N] {

      case class RefNode(node: N, id: UUID = UUID.randomUUID()) extends _RefNode {

        {
          sameRefs
        }

        lazy val refKeyOpt: Option[Any] = sameRefBy(node)

        lazy val (sameRefs: SameRefs, shouldExpand: Boolean) = {

          refKeyOpt
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
            .getOrElse {
              SameRefs() -> true
            }
        }

        lazy val bindingOpt: Option[String] = {

          refKeyOpt.flatMap { k =>
            binded.get(k)
          }
        }

      }

      case class RefTree(node: N) extends Tree[RefNode] {

        override lazy val root: RefNode = RefNode(node)

        case class Ops(override val node: RefNode) extends UpperNOps {

          lazy val originalOps: graph.OutboundNOps = graph.nodeOps(node.node)

          override protected def getInduction: Seq[Arrow.`~>`.NoInfo[RefNode]] = {

            if (!node.shouldExpand) {
              Nil
            } else {

              val result = originalOps.induction.map { arrow =>
                Arrow.`~>`.NoInfo(RefNode(arrow.target)) // this discard info from arrows
              }

              result
            }
          }

          override protected def getNodeText: String = {

            val originalText = originalOps.nodeText

            node.bindingOpt
              .map { binding =>
                addAnnotation(originalText, binding)
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
