package ai.acyclic.prover.commons.jit.eval

trait Conformal[I <: Args] {

  type NoInput <: I
  val noInput: NoInput

  def getNoInput[T <: I]: T = noInput.asInstanceOf[T]
}
