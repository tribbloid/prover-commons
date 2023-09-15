package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.ops.AnyGraphUnary
import ai.acyclic.prover.commons.typesetting.TextBlock

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.language.existentials

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

  type Graph_/\[V] = Local.AnyGraph.Outbound[V]

  implicit lazy val defaultFormat: Default = new Default(Hierarchy.default)

  implicit def newGroup: defaultFormat.Group = defaultFormat.Group()

  class Default(
      val backbone: Hierarchy
  ) extends LinkedHierarchy {

    override def dryRun(tree: Local.Tree[_ <: RefBindingLike]): Unit = {
      AnyGraphUnary
        .^(tree, backbone.maxDepth)
        .Traverse(
          down = { n =>
            n.induction
          }
        )
        .DepthFirst
        .compute
    }
  }

  trait RefBindingLike {

    def original: Any
  }

  type SameRefs = mutable.ArrayBuffer[RefBindingLike]
  def emptySameRefs = mutable.ArrayBuffer.empty[RefBindingLike]
}

trait LinkedHierarchy extends LinkedHierarchy.Format {

  import LinkedHierarchy._

  def backbone: Hierarchy

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  def dryRun(tree: Local.Tree[_ <: RefBindingLike]): Unit

  trait _TextViz[V] extends TextViz[V]

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    lazy val bindingIndices = new AtomicInteger(0)

    case class Viz[V](override val semilattice: Graph_/\[V]) extends _TextViz[V] {

      object RefBindings extends Local.Tree.UntypedDef {

        case class Node(
            override val original: Local.AnyGraph.Outbound.Node[V],
            id: UUID = UUID.randomUUID()
        ) extends UntypedNode
            with RefBindingLike {

          {
            sameRefs_shouldExpand
          }

          lazy val refKeyOpt: Option[Any] = original.identityKey

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

                    val result = emptySameRefs
                    result += this

                    expanded.put(refKey, result)
                    result -> true
                  }
                result
              }

            val result = existing
              .getOrElse {
                emptySameRefs -> true
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

          override protected def inductionC: Seq[(Arrow.`~>`.^, RefBindings.Node)] = {

            val result = if (!shouldExpand) {
              Nil
            } else {

              original.induction.map { tuple =>
                val arrow = tuple._1: Arrow.`~>`.^

                val target: RefBindings.Node = RefBindings.Node.apply(tuple._2)

                arrow -> target
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

      lazy val delegates: Seq[Local.Tree[RefBindings.Node]] = {
        val roots: Vector[Local.AnyGraph.Outbound.Node[V]] = semilattice.entriesC
        roots.map { node =>
          val refBinding: RefBindings.Node = RefBindings.Node(node)

          Local.Tree.makeExact(refBinding)
        }
      }

      def dryRun(): Unit = {

        delegates.foreach { g =>
          LinkedHierarchy.this.dryRun(g)
        }
      }

      override lazy val toString: String = {
        dryRun()

        delegates
          .map { v =>
            backbone.Viz(v).toString
          }
          .mkString("\n")
      }
    }
  }
}
