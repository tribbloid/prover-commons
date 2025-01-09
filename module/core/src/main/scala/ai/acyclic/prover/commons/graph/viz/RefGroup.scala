package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.local.Local.Diverging
import ai.acyclic.prover.commons.typesetting.TextBlock

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object RefGroup {

  trait RefNode {

    def original: Any
  }

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
}

/**
  * this can convert any graph into a Reference Poset with no cycle: upon detecting a cycle at a node, the node is
  * splitted into
  *
  *   - 1 onwer, full dependent
  *   - mutliple pointers, no dependent
  */
case class RefGroup() {

  import RefGroup.*

  lazy val bindingIndices = new AtomicInteger(0)

  case class RefCounting() {

    lazy val bindings: ArrayBuffer[RefNode] = mutable.ArrayBuffer.empty[RefNode]

    @volatile var nameOpt: Option[String] = None
  }

  lazy val refCountings: mutable.LinkedHashMap[Any, RefCounting] = mutable.LinkedHashMap.empty

  def convertNode[V](source: Diverging.Graph.Node[V]): Node = RefDomain.node(source)

  def convertGraph[V](source: Local.Diverging.Graph[V]): Local.Diverging.Poset.Graph[RefDomain.node] = {
    // TODO: more engine

    val refs: Local.Batch[Node] = source.entries.map { v =>
      convertNode(v)
    }

    Local.Diverging.Poset.buildExact(refs)
  }

  type Node = RefDomain.node
  object RefDomain extends Diverging.Poset.Codomain {

    case class node(
        override val original: Diverging.Graph.Node[?],
        id: UUID = UUID.randomUUID() // TODO: need to get rid of this and move to evalCacheKey
    ) extends Node_
        with RefNode {

      {
        // for RefNode all inductions have to be eagerly computed to build a complete refCountings
        inductions
        refCounting_isOwner
      }

//      override def evalCacheKeyC: Option[Any] = original.evalCacheKey

      lazy val refKeyOpt: Option[Any] = original.identity

      lazy val refCounting_isOwner: (RefCounting, Boolean) = {

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

      def isOwner: Boolean = refCounting_isOwner._2

      def bindingNameOpt: Option[String] = {
        refKeyOpt.flatMap { refKey =>
          refCountings.get(refKey).flatMap(_.nameOpt)
        }
      }

      override lazy val inductions: Seq[(Arrow.OutboundT.^, RefDomain.node)] = {

        val result = if (!isOwner) {
          Nil
        } else {

          original.inductions.map { tuple =>
            val arrow = tuple._1: Arrow.Outbound

            val target: RefDomain.node = RefDomain.node(tuple._2)

            arrow -> target
          }
        }

        result
      }

      override lazy val nodeText: String = {

        val originalText = original.nodeText

        val result = bindingNameOpt
          .map { name =>
            if (isOwner) addSrcAnnotation(originalText, name)
            else addRefAnnotation(originalText, name)
          }
          .getOrElse(originalText)

        result
      }
    }
  }

}
