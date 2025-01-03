package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.util.Magnet.OptionMagnet

import scala.collection.Factory
import scala.language.implicitConversions

// TODO: may represents undesirable relationship.
//  should define a link type equivalent to (Arrow, Node)
trait Arrow {

  import Arrow.*

  val arrowType: ArrowType

  protected def arrowTextC: Option[String] = None
  final lazy val arrowText: Option[String] = arrowTextC
}

object Arrow {

  sealed trait ArrowType extends Product {

    trait ^ extends Arrow {

      override val arrowType: ArrowType.this.type = ArrowType.this
    }

    trait NoInfo extends ^ {

      final override def arrowTextC: None.type = None

      case class OfText(text: OptionMagnet[String]) extends ^ {

        @transient final override lazy val arrowTextC: Option[String] = text
      }
    }
    object NoInfo extends NoInfo {}

    // TODO: this should be a magnet
    implicit def pair[N](v: N): (^, N) = NoInfo -> v

    implicit def pairMany[F[T] <: Iterable[T], N](vs: F[N])(
        implicit
        toF: Factory[(^, N), F[(^, N)]]
    ): F[(^, N)] = {
      val mapped = vs.map { v =>
        pair(v)
      }
      toF.fromSpecific(mapped)
    }
  }

  sealed abstract class Edge extends ArrowType {}

  case object OutboundT extends Edge

  type Outbound = OutboundT.^
  val Outbound = OutboundT.NoInfo
  type `~>` = Outbound
  val `~>` = Outbound

  implicitly[`~>`.type <:< `~>`]

  case object InboundT extends Edge
  type Inbound = InboundT.^
  val Inbound = InboundT.NoInfo
  type `<~` = Inbound
  val `<~` = Inbound

  implicitly[`<~`.type <:< `<~`]

//  case object `~~` extends Edge // undirected

  // used to expand a graph without introducing new edge
  // e.g. expanding a semilattice into a poset
  sealed abstract class NonEdge extends ArrowType

//  case object Discovery extends NonEdge
}
