package ai.acyclic.prover.commons.graph

import scala.collection.Factory
import scala.language.implicitConversions

// TODO: should this be within a System to enable family polymorphism?
trait Arrow {

  import Arrow._

  val target: Any

  val arrowType: ArrowType

  lazy val arrowText: Option[String] = None
}

object Arrow {

  trait Of[+N] extends Arrow {

    override val target: N
  }

  trait NoInfoLike extends Arrow

  trait ArrowType extends Product {

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

    case class NoInfo[N](override val target: N) extends Of[N] with NoInfoLike {}

    object NoInfo {}
  }

  abstract class Edge extends ArrowType {}

  case object `~>` extends Edge

  case object `<~` extends Edge

  case object `~~` extends Edge // undirected

  // used to expand a graph without introducing new edge
  // e.g. expanding a semilattice into a poset
  abstract class NonEdge extends ArrowType
}
