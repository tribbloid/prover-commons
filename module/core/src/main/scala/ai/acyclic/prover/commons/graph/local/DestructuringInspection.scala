package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.reflect.ClassTag

object DestructuringInspection {

  case class Destructured[+I](
      prefix: String,
      induction: Seq[I],
      contents: Seq[Any]
  )
}

abstract class DestructuringInspection[
    I, // inductively applicable
    T <: I // immediately applicable
](
    implicit
    tI: ClassTag[I]
) extends Local.Semilattice.Upper.Inspection[I] {

  import DestructuringInspection.*

  def unapplyAll(value: I): Destructured[I] = Destructured(value.toString, Nil, Nil)

  //  def unapplyContent(value: I): Option[Seq[Any]] = None
  // product element is either an content or a node, they are all subnode by default

  type node = NodeImpl

  abstract class NodeImpl(value: I) extends _Node {

    protected val decomposed: Destructured[I] = unapplyAll(value)

    override protected lazy val getInduction: Seq[(_Arrow, node)] = {

      implicitly[_Arrow <:< Arrow.`~>`]
      implicitly[_Arrow =:= Arrow.`~>`]

      val result: Seq[(_Arrow, node)] = decomposed.induction.map { v =>
        Arrow.`~>` -> node(v)
      }

      result
    }
  }

  case class BuiltIn(value: I) extends NodeImpl(value) {

    final override protected def getNodeText: String = {

      val prefix = decomposed.prefix

      if (decomposed.contents.isEmpty) {

        prefix
      } else {

        val _innerBlocks: Seq[String] = decomposed.contents.map { v =>
          "" + v
        }

        prefix + _innerBlocks.mkString("(", ", ", ")")
      }
    }
  }

  case class Long(value: I) extends NodeImpl(value) {

    final override protected def getNodeText: String = {

      val prefixElements: Vector[String] = {

        val outers = HasOuter.outerListOf(value)
        val names = outers.collect {
          case v: Product => v.productPrefix
        }.toVector

        names.reverse
      }

      val prefix = prefixElements.mkString(" ‣ ")

      if (decomposed.contents.isEmpty) {

        prefix
      } else {

        val _innerBlocks = decomposed.contents.map { v =>
          TextBlock("" + v).pad.left(Padding.argLeftBracket).build
        }

        TextBlock(prefix)
          .zipRight(
            TextBlock(_innerBlocks.mkString("\n"))
          )
          .build
      }
    }
  }
}
