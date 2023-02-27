package ai.acyclic.prover.commons.graph

import scala.collection.Factory
import scala.language.implicitConversions

// TODO: should this be within a System to enable family polymorphism?
trait Arrow {

  import Arrow._

  val target: Any

  val arrowType: ArrowType

  protected def getArrowText: Option[String] = None
  final lazy val arrowText: Option[String] = getArrowText
}

object Arrow {

  trait Of[+N] extends Arrow {

    override val target: N
  }

  trait NoInfoLike extends Arrow

  sealed trait ArrowType extends Product {

    trait Of[+N] extends Arrow.Of[N] {

      override val arrowType: ArrowType.this.type = ArrowType.this
    }

    object Of {

      implicit def defaultsTo[N](v: N): NoInfo[N] = NoInfo[N](v)

      implicit def defaultToMany[F[T] <: Iterable[T], N](vs: F[N])(
          implicit
          toF: Factory[NoInfo[N], F[NoInfo[N]]]
      ): F[NoInfo[N]] = {
        val mapped: Iterable[NoInfo[N]] = vs.map { v =>
          defaultsTo(v)
        }
        toF.fromSpecific(mapped)
      }
    }

    case class NoInfo[N](
        override val target: N,
        override val getArrowText: Option[String] = None
    ) extends Of[N]
        with NoInfoLike {}

    object NoInfo {}
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
