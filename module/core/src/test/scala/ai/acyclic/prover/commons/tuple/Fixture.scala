package ai.acyclic.prover.commons.tuple

object Fixture {

  object SrcSystem extends Products.Monoidal {
    type VBound = Any
    type Element[T <: VBound] = T

    override def cons[L <: VBound, TAIL <: Prod](head: L, tail: TAIL): L ><: TAIL = ><:(head, tail)
    override def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (L, TAIL) = (cons.head, cons.tail)

    sealed trait Prod
    case object Atom extends Prod
    type Eye = Atom.type
    override val Eye: Eye = Atom

    case class ><:[L <: VBound, TAIL <: Prod](head: L, tail: TAIL) extends Prod
  }

  object TgtSystem extends Products.Monoidal {
    type VBound = Any
    type Element[T <: VBound] = T
    override def cons[L <: VBound, TAIL <: Prod](head: L, tail: TAIL): L ><: TAIL = ><:(head, tail)
    override def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (L, TAIL) = (cons.head, cons.tail)

    sealed trait Prod
    case object Atom extends Prod
    type Eye = Atom.type
    override val Eye: Eye = Atom

    case class ><:[L <: VBound, TAIL <: Prod](head: L, tail: TAIL) extends Prod
  }
}
