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

  def getInners(value: I): Vector[Any] =
    Vector.empty // product element is either an arg or a node, they are all subnode by default

  case class node(value: I) extends _Node {

    private object _inners {

      val vec: Vector[Any] = getInners(value)
      val set = vec.toSet
    }

    private lazy val allPrefixes: Vector[String] = {

      val outers = HasOuter.outerListOf(value)
      val all = Vector(value) ++ outers
      val names = all.collect {
        case v: Product => v.productPrefix
      }

      names.reverse
    }

    final override protected def getNodeText: String = {

      val fullPrefix = allPrefixes.mkString(" ‣ ")

      if (_inners.vec.isEmpty) {

        fullPrefix
      } else {

        val _innerBlocks = _inners.vec.map { str =>
          TextBlock("" + str).pad.left(Padding.argLeftBracket).build
        }

        TextBlock(fullPrefix)
          .zipRight(
            TextBlock(_innerBlocks.mkString("\n"))
          )
          .build
      }
    }

    override protected lazy val getInduction: Seq[(Arrow.`~>`, node)] = {

      val elements = value.productIterator.to(LazyList)

      val result: Seq[(_Arrow, node)] = elements.flatMap {
        case element: I =>
          if (_inners.set.contains(element)) Nil
          else {

            val subNode = node(element)

            Some(Arrow.`~>` -> subNode)
          }

        case _ =>
          Nil
      }

      result
    }
  }
}
