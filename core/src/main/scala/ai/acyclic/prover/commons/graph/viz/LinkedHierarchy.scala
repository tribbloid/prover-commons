package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Topology.TreeT
import ai.acyclic.prover.commons.graph.local.Local
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

  type UB[V] = Local.Graph.Outbound[V]

  implicit lazy val defaultFormat: Default = new Default(Hierarchy.default)

  implicit def newGroup: defaultFormat.Group = defaultFormat.Group()

  class Default(
      val backbone: Hierarchy
  ) extends LinkedHierarchy {

    override def sameRefBy(node: Local.Graph.Outbound.Node[_]): Option[Any] = Some(node)

    override def dryRun(tree: Local.Tree[_ <: _RefBinding]): Unit = {
      GraphUnary
        .^(tree)
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

    def original: Any
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

  def dryRun(tree: Local.Tree[_ <: _RefBinding]): Unit

  def sameRefBy(node: Local.Graph.Outbound.Node[_]): Option[Any]

  trait _Viz[V] extends TextViz[V]

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    lazy val bindingIndices = new AtomicInteger(0)

    case class Viz[V](override val semilattice: UB[V]) extends _Viz[V] {

      object RefBindingT extends Local.Tree.UntypedDef {

        case class Node(
            original: Local.Graph.Outbound.Node[V],
            id: UUID = UUID.randomUUID()
        ) extends UntypedNode
            with _RefBinding {

          override val law: TreeT._L = Local.Tree.law

          {
            sameRefs_shouldExpand
          }

          lazy val refKeyOpt: Option[Any] = sameRefBy(original)

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

          override protected def inductionC: Seq[(_A, RefBindingT.Node)] = {

            val result: Seq[(_A, RefBindingT.Node)] = if (!shouldExpand) {
              Nil
            } else {

              original.induction.map { v =>
                (v._1: Local.Tree.law._A) -> RefBindingT.Node.apply(v._2)
              }

            }

            result
          }

          override protected def nodeTextC: String = {

            val originalText = original.nodeText

            bindingNameOpt
              .map { name =>
                if (shouldExpand) addSrcAnnotation(originalText, name)
                else addRefAnnotation(originalText, name)
              }
              .getOrElse(originalText)
          }
        }
      }

      lazy val delegates: Seq[Local.Tree[RefBindingT.Node]] = {
        val roots: Vector[Local.Graph.Outbound.Node[V]] = semilattice.entriesC
        roots.map { node =>
          val refBinding: RefBindingT.Node = RefBindingT.Node(node)
          Local.Tree(refBinding)
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
