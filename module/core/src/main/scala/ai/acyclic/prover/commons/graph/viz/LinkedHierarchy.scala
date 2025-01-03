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

    override def dryRun(tree: Local.Tree[? <: RefBindingLike]): Unit = {

      val _tree = tree.asAnyGraphOps
      _tree
        .Traverse(
          down = { n =>
            n.inductions
          }
        )
        .DepthFirst
    }
  }

  object Default extends Default(Hierarchy.Default)

  implicit def newGroup: Default.Group = Default.Group()

  trait RefBindingLike {

    def original: Any
  }

}

abstract class LinkedHierarchy extends Visualisation.OfType(Local.AnyGraph.Outbound) {

  import LinkedHierarchy.*

  def __sanity[T](): Unit = {

    implicitly[MaxGraph[T] =:= Local.AnyGraph.Outbound[T]]
    implicitly[MaxNode[T] =:= Local.AnyGraph.Outbound.Node[T]]
  }

  def backbone: Hierarchy

  lazy val bindings: LazyList[String] = (0 until Int.MaxValue).to(LazyList).map(v => "" + v)

  protected def dryRun(tree: Local.Tree[? <: RefBindingLike]): Unit

  final override def show[V](data: MaxGraph[V]): Visual[V] = Group().Viz(data)

  // shared between visualisations of multiple graphs
  case class Group() {

    lazy val bindingIndices = new AtomicInteger(0)

    case class RefCounting() {

      lazy val bindings = mutable.ArrayBuffer.empty[RefBindingLike]

      var nameOpt: Option[String] = None
    }

    lazy val refCountings: mutable.LinkedHashMap[Any, RefCounting] = mutable.LinkedHashMap.empty

    def visualize[V](data: Local.AnyGraph.Outbound[V]): Visual[V] = Viz(data)

    case class Viz[V](override val unbox: MaxGraph[V]) extends Visual[V] {

      object RefBindings extends Local.Tree.NodeGroup {

        case class node(
            override val original: Local.AnyGraph.Outbound.Node[V],
            id: UUID = UUID.randomUUID() // TODO: need to get rid of this
        ) extends NodeInGroup
            with RefBindingLike {

          {
            refCounting_shouldExpand
          }

          lazy val refKeyOpt: Option[Any] = original.identity

          lazy val refCounting_shouldExpand: (RefCounting, Boolean) = {

            val existing = refKeyOpt
              .map { refKey =>
                val result = refCountings
                  .get(refKey)
                  .map { rc =>
                    rc.nameOpt.getOrElse(
                      rc.nameOpt = Some(bindingIndices.getAndIncrement().toString)
                    )

                    rc.bindings += this
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

          lazy val bindingNameOpt: Option[String] = {
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

            bindingNameOpt
              .map { name =>
                if (shouldExpand) addSrcAnnotation(originalText, name)
                else addRefAnnotation(originalText, name)
              }
              .getOrElse(originalText)
          }
        }
      }

      lazy val delegates: Local.Batch[Local.Tree[RefBindings.node]] = {
        unbox.entries.map { node =>
          val refBinding: RefBindings.node = RefBindings.node(node)

          Local.Tree.makeExact(refBinding)
        }
      }

      def dryRun(): Unit = {

        delegates.collect.foreach { g =>
          LinkedHierarchy.this.dryRun(g)
        }
      }

      override lazy val toString: String = {
        dryRun()

        delegates
          .map { v =>
            backbone.Viz(v).toString
          }
          .collect
          .mkString("\n")
      }
    }
  }
}
