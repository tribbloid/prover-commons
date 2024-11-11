package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.ops.AnyGraphUnary
import ai.acyclic.prover.commons.typesetting.TextBlock

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object LinkedHierarchy {

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

  class Default(
      val backbone: Hierarchy
  ) extends LinkedHierarchy {

    override def dryRun(tree: Local.Tree[? <: RefBindingLike]): Unit = {
      val unary = {
        AnyGraphUnary
          .^(tree, backbone.maxDepth)
      }

      unary
        .Traverse(
          down = { n =>
            n.induction
          }
        )
        .DepthFirst
        .compute
    }

  }

  object Default extends Default(Hierarchy.Default)

  implicit def newGroup: Default.Group = Default.Group()

  trait RefBindingLike {

    def original: Any
  }

  type SameRefs = mutable.ArrayBuffer[RefBindingLike]
  def emptySameRefs = mutable.ArrayBuffer.empty[RefBindingLike]
}

trait LinkedHierarchy extends Visualisation.OfType {

  import LinkedHierarchy.*

  final override val applicableToType: Local.AnyGraph.Outbound.type = Local.AnyGraph.Outbound

  def __sanity[T](): Unit = {

    implicitly[Graph_/\[T] =:= Local.AnyGraph.Outbound[T]]
    implicitly[Node_/\[T] =:= Local.AnyGraph.Outbound.Node[T]]
  }

  def backbone: Hierarchy

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  protected def dryRun(tree: Local.Tree[? <: RefBindingLike]): Unit

  final override def visualise[V](data: Graph_/\[V]): Visualized[V] = Group().Viz(data)

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val expanded: mutable.LinkedHashMap[Any, SameRefs] = mutable.LinkedHashMap.empty

    lazy val binded: mutable.LinkedHashMap[Any, String] = mutable.LinkedHashMap.empty

    lazy val bindingIndices = new AtomicInteger(0)

    def visualize[V](data: Local.AnyGraph.Outbound[V]): Visualized[V] = Viz(data)

    case class Viz[V](override val data: Graph_/\[V]) extends Visualized[V] {

      object RefBindings extends Local.Tree.Group {

        case class node(
            override val original: Local.AnyGraph.Outbound.Node[V],
            id: UUID = UUID.randomUUID()
        ) extends _Node
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

          override protected def getInduction: Seq[(Arrow.Outbound.^, RefBindings.node)] = {

            val result = if (!shouldExpand) {
              Nil
            } else {

              original.induction.map { tuple =>
                val arrow = tuple._1: Arrow.Outbound.^

                val target: RefBindings.node = RefBindings.node.apply(tuple._2)

                arrow -> target
              }

            }

            result
          }

          override protected def getNodeText: String = {

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

      lazy val delegates: Seq[Local.Tree[RefBindings.node]] = {
        val roots: Vector[Local.AnyGraph.Outbound.Node[V]] = data.getEntries
        roots.map { node =>
          val refBinding: RefBindings.node = RefBindings.node(node)

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
