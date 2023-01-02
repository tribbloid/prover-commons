package ai.acyclic.prover.commons.graph

// TODO: should this be within a System to enable family polymorphism?
trait Arrow {

  import Arrow._

  val target: Graph._Node

  val arrowType: ArrowType

  lazy val arrowText: Option[String] = None
}

object Arrow {

  trait CanUpdate[THIS <: Arrow, THAT <: Arrow, N <: Graph._Node] {

    trait ThisOps {

      def updateTarget(n: N): THAT
    }

    type Ops <: ThisOps
    def Ops: THIS => Ops
  }

  trait Of[+T <: Graph._Node] extends Arrow {

    override val target: T
  }

  trait ArrowType extends Product {

    trait Of[+T <: Graph._Node] extends Arrow.Of[T] {

      override val arrowType: ArrowType.this.type = ArrowType.this
    }

    case class NoInfo[T <: Graph._Node](override val target: T) extends Of[T] {}

    object NoInfo {

      case class _CanUpdate[A <: Graph._Node, B <: Graph._Node]() extends CanUpdate[NoInfo[A], NoInfo[B], B] {

        case class Ops(arrow: NoInfo[A]) extends ThisOps {

          override def updateTarget(n: B): NoInfo[B] = {
            arrow.copy(n)
          }
        }
      }

      implicit def updater[A <: Graph._Node, B <: Graph._Node]: _CanUpdate[A, B] = _CanUpdate()
    }
  }

  abstract class Edge extends ArrowType {}

  case object `~>` extends Edge

  case object `<~` extends Edge

  case object `~~` extends Edge // undirected

  // used to expand a graph without introducing new edge
  // e.g. expanding a semilattice into a poset
  abstract class NonEdge extends ArrowType
}
