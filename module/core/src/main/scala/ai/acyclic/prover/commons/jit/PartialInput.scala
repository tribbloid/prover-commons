package ai.acyclic.prover.commons.jit

trait PartialInput[T] {}

object PartialInput {

  case class Provided[T](v: T) extends PartialInput[T]

  case class Janky[T](fn: () => T) extends PartialInput[T]
}
