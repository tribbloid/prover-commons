package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
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

    override def scanReferences(tree: Local.Diverging.Tree[RefBindingLike]): Unit = {

      val _tree = tree.ops_anyGraph
      _tree
        .Traverse(
          down = { n =>
            n.inductions
          }
        )
        .DepthFirst
    }

    override def maxRecursionDepth: Int = backbone.maxRecursionDepth
  }

  object Default extends Default(Hierarchy.Default)

  implicit def newGroup: Default.Group = Default.Group()

  trait RefBindingLike {

    def original: Any
  }

}

abstract class LinkedHierarchy extends Visualisation.Local(Local.Diverging.Graph) {

  import LinkedHierarchy.*

  def __sanity[T](): Unit = {

    implicitly[MaxGraph[T] =:= Local.Diverging.Graph[T]]
    implicitly[MaxNode[T] =:= Local.Diverging.Graph.Node[T]]
  }

  def backbone: Hierarchy

  def scanReferences(tree: Local.Diverging.Tree[RefBindingLike]): Unit

  final override def show[V](data: MaxGraph[V]): Visual[V] = Group().Viz(data)

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val bindingIndices = new AtomicInteger(0)

    case class RefCounting() {

      lazy val bindings = mutable.ArrayBuffer.empty[RefBindingLike]

      @volatile var nameOpt: Option[String] = None
    }

    lazy val refCountings: mutable.LinkedHashMap[Any, RefCounting] = mutable.LinkedHashMap.empty

    def visualize[V](data: Local.Diverging.Graph[V]): Visual[V] = Viz(data)

    object RefBindings extends Local.Diverging.Tree.Codomain {

      case class node(
          override val original: Local.Diverging.Graph.Node[?],
          id: UUID = UUID.randomUUID() // TODO: need to get rid of this
      ) extends Node_
          with RefBindingLike {

        {
          refCounting_shouldExpand
        }

        lazy val refKeyOpt: Option[Any] = original.identity

        lazy val refCounting_shouldExpand: (RefCounting, Boolean) = {

          val existing = refKeyOpt
            .map { refKey =>
              val rcOpt = refCountings.get(refKey)

              val result = rcOpt
                .map { rc =>
                  rc.bindings += this
                  if (rc.nameOpt.isEmpty) rc.nameOpt = Some(bindingIndices.getAndIncrement().toString)
                  rc -> false
                }
                .getOrElse {

                  val result = RefCounting()
                  result.bindings += this

                  refCountings.put(refKey, result)
                  result -> true
                }
              result
            }

          val result = existing
            .getOrElse {
              RefCounting() -> true
            }
          result
        }

        def shouldExpand: Boolean = refCounting_shouldExpand._2

        def bindingNameOpt: Option[String] = {
          refKeyOpt.flatMap { refKey =>
            refCountings.get(refKey).flatMap(_.nameOpt)
          }
        }

        override lazy val inductions: Seq[(Arrow.OutboundT.^, RefBindings.node)] = {

          val result = if (!shouldExpand) {
            Nil
          } else {

            original.inductions.map { tuple =>
              val arrow = tuple._1: Arrow.Outbound

              val target: RefBindings.node = RefBindings.node(tuple._2)

              arrow -> target
            }

          }

          result
        }

        override lazy val nodeText: String = {

          val originalText = original.nodeText

          val result = bindingNameOpt
            .map { name =>
              if (shouldExpand) addSrcAnnotation(originalText, name)
              else addRefAnnotation(originalText, name)
            }
            .getOrElse(originalText)

          result
        }
      }
    }

    // TODO: no need,
    case class Viz[V](override val unbox: MaxGraph[V]) extends Visual[V] {

      lazy val refBindingBatch: Local.Batch[Local.Diverging.Tree[RefBindings.node]] = {
        unbox.entries.map { node =>
          val refBinding: RefBindings.node = RefBindings.node(node)

          Local.Diverging.Tree.makeExact(refBinding)
        }
      }

      lazy val scanReferencesOnce: Unit = {

        refBindingBatch.collect.foreach { g =>
          LinkedHierarchy.this.scanReferences(g)
        }
      }

      override lazy val text: String = {
        scanReferencesOnce

        refBindingBatch.collect
          .map { v =>
            backbone.Viz(v).toString
          }
          .mkString("\n")
      }
    }
  }
}
