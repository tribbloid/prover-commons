package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.HasInner
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.multiverse.CanUnapply

object UnapplyInspection {}

trait UnapplyInspection extends Local.Diverging.UpperSemilattice.topologyImpls.Inspection[Any] with HasInner {

  def primary: CanUnapply[Any] // for induction
  def inlined: CanUnapply[Any] // for content

  @transient lazy val primaryEffective: CanUnapply.Quotient[Any] = primary / inlined
  // if already part of the content, do not use in induction

//  lazy val secondary_effective: CanUnapply.Quotient[Any] =
//    secondary / primary // if already an induction, no need to show as content

//  type node = _Node

  object Node {

    trait Minimal extends Node_ with _Inner {

      override lazy val evalCacheKey: Option[Any] = Some(value)

      @transient lazy val primaryFormOpt = primaryEffective.unapply(value)

      def prefix: String = primaryFormOpt.map(_.prefix).getOrElse(value.toString)

      override lazy val inductions: Seq[(_Arrow, Node_)] = {

        val inductions = primaryFormOpt.map(_.kvPairs).getOrElse(Nil)

        val result: Seq[(_Arrow, Node_)] = inductions.map { kv =>
          Arrow.`~>` -> inspect(kv._2)
        }

        result
      }
    }

    trait Named extends Minimal {

      override lazy val inductions: Seq[(_Arrow, Node_)] = {

        val inductions = primaryFormOpt.map(_.kvPairs).getOrElse(Nil)

        val result: Seq[(_Arrow, Node_)] = inductions.map { kv =>
          Arrow.`~>`.OfText(kv._1) -> inspect(kv._2)
        }

        result
      }
    }

    // TODO: implement
    //  case class Long(value: Value) extends NodeImpl(value) {
    //
    //    final override protected def getNodeText: String = {
    //
    //      val prefixElements: Vector[String] = {
    //
    //        val outers = HasInner.outerListOf(value)
    //        val names = outers.collect {
    //          case v: Product => v.productPrefix
    //        }.toVector
    //
    //        names.reverse
    //      }
    //
    //      val prefix = prefixElements.mkString(" ‣ ")
    //
    //      if (decomposed.contents.isEmpty) {
    //
    //        prefix
    //      } else {
    //
    //        val _innerBlocks = decomposed.contents.map { v =>
    //          TextBlock("" + v).pad.left(Padding.argLeftBracket).build
    //        }
    //
    //        TextBlock(prefix)
    //          .zipRight(
    //            TextBlock(_innerBlocks.mkString("\n"))
    //          )
    //          .build
    //      }
    //    }
    //  }
  }

  case class Node(value: Any) extends Node.Minimal {

    lazy val inlinedFormOpt = inlined.unapply(value)

    @transient final override lazy val nodeText: String = {

      val contents: Seq[Any] = inlinedFormOpt.map(_.values).getOrElse(Nil)

      if (contents.isEmpty) {

        prefix
      } else {

        val _innerBlocks: Seq[String] = contents.map { v =>
          "" + v
        }

        prefix + _innerBlocks.mkString("(", ", ", ")")
      }
    }
  }

  override lazy val inspect: Any => ProductInspection.Node_ = { v =>
    new Node(v)
  }
}
