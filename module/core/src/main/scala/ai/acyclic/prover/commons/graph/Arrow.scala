package ai.acyclic.prover.commons.graph

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

  trait NoInfoLike extends Arrow

  sealed trait ArrowType extends Product {

    trait ^ extends Arrow {

      override val arrowType: ArrowType.this.type = ArrowType.this
    }

    // TODO: this should be a magnet
    implicit def pair[N](v: N): (^, N) = NoInfo() -> v

    implicit def pairMany[F[T] <: Iterable[T], N](vs: F[N])(
        implicit
        toF: Factory[(^, N), F[(^, N)]]
    ): F[(^, N)] = {
      val mapped = vs.map { v =>
        pair(v)
      }
      toF.fromSpecific(mapped)
    }

    case class NoInfo(
        override protected val arrowTextC: Option[String] = None
    ) extends ^
        with NoInfoLike {}

    object NoInfo {

      lazy val empty = NoInfo()
    }

//    object NoInfo {
//
////      lazy val empty: NoInfo = NoInfo()
//    }
  }

  sealed abstract class Edge extends ArrowType {}

  case object Outbound extends Edge
  type `~>` = Outbound.^
  object `~>` extends Outbound.NoInfo() {}

  implicitly[`~>`.type <:< `~>`]

  case object Inbound extends Edge
  type `<~` = Inbound.^
  object `<~` extends Inbound.NoInfo() {}

  implicitly[`<~`.type <:< `<~`]

//  case object `~~` extends Edge // undirected

  // used to expand a graph without introducing new edge
  // e.g. expanding a semilattice into a poset
  sealed abstract class NonEdge extends ArrowType

//  case object Discovery extends NonEdge
}
