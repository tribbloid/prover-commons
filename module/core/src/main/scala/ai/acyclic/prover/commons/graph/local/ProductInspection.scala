package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.reflect.ClassTag

abstract class ProductInspection[
    I <: Product, // inductively applicable
    T <: I // immediately applicable
](
    implicit
    tI: ClassTag[I]
) extends Local.Semilattice.Upper.Inspection[I] {

  def unapply(value: I): Option[Seq[Any]] = None
  // product element is either an arg or a node, they are all subnode by default

  type node = NodeImpl

  abstract class NodeImpl(value: I) extends _Node {

    protected object Resolving {

      val content: Seq[Any] = unapply(value).getOrElse(Nil)
      val contentSet = content.toSet

      val inductions_inhabitants = {
        val elements = value.productIterator.to(LazyList)

        val result: LazyList[Either[I, Any]] = elements.flatMap { v =>
          if (Resolving.contentSet.contains(v)) Nil
          else {
            v match {
              case sub: I =>
                if (Resolving.contentSet.contains(sub)) Nil
                else {

                  Seq(Left(sub))
                }

              case _ =>
                Seq(Right(v))
            }
          }
        }
        result
      }

      val induction: LazyList[I] = inductions_inhabitants.collect {
        case Left(v) => v
      }

      val inhabitants: LazyList[Any] = inductions_inhabitants.collect {
        case Right(v) => v
      }
    }

    override protected lazy val getInduction: Seq[(_Arrow, node)] = {

      this.axioms

      implicitly[_Arrow <:< Arrow.`~>`]
      implicitly[_Arrow =:= Arrow.`~>`]

      val result: Seq[(_Arrow, node)] = Resolving.induction.map { v =>
        Arrow.`~>` -> node(v)
      }

      result
    }
  }

  case class BuiltIn(value: I) extends NodeImpl(value) {

    final override protected def getNodeText: String = {

      val prefix = value.productPrefix

      if (Resolving.inhabitants.isEmpty) {

        prefix
      } else {

        val _innerBlocks: Seq[String] = Resolving.inhabitants.map { v =>
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

      if (Resolving.inhabitants.isEmpty) {

        prefix
      } else {

        val _innerBlocks = Resolving.inhabitants.map { v =>
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
