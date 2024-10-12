package ai.acyclic.prover.commons.graph

import scala.collection.Factory
import scala.language.implicitConversions

// TODO: may represents undesirable relationship.
//  should define a link type equivalent to (Arrow, Node)
trait Arrow {

  import Arrow._

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

    implicit def pair[N](v: N): (NoInfo, N) = NoInfo.empty -> v

    implicit def pairMany[F[T] <: Iterable[T], N](vs: F[N])(
        implicit
        toF: Factory[(NoInfo, N), F[(NoInfo, N)]]
    ): F[(NoInfo, N)] = {
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
  }

  sealed abstract class Edge extends ArrowType {}

  case object Outbound extends Edge
  type `~>` = Outbound.^
  val `~>` = Outbound.NoInfo.empty

  implicitly[`~>`.type <:< `~>`]

  case object Inbound extends Edge
  type `<~` = Inbound.^
  val `<~` = Inbound.NoInfo.empty

  implicitly[`<~`.type <:< `<~`]

//  case object `~~` extends Edge // undirected

  // used to expand a graph without introducing new edge
  // e.g. expanding a semilattice into a poset
  sealed abstract class NonEdge extends ArrowType

  case object Discovery extends NonEdge
}
