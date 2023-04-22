package ai.acyclic.prover.commons.graph

import scala.collection.Factory
import scala.language.implicitConversions

trait Arrow {

  import Arrow._

  val arrowType: ArrowType

  protected def getArrowText: Option[String] = None
  final lazy val arrowText: Option[String] = getArrowText
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

    object ^ {}

    case class NoInfo(
        override val getArrowText: Option[String] = None
    ) extends ^
        with NoInfoLike {}

    object NoInfo {

      lazy val empty = NoInfo()
    }
  }

  sealed abstract class Edge extends ArrowType {}

  case object `~>` extends Edge

  case object `<~` extends Edge

//  case object `~~` extends Edge // undirected

  // used to expand a graph without introducing new edge
  // e.g. expanding a semilattice into a poset
  sealed abstract class NonEdge extends ArrowType

  case object Discovery extends NonEdge
}
