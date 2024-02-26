package ai.acyclic.prover.commons.jit

// should have 1 definition for Fn and Poly
trait Bijection[A, B] {

  def apply(v: A): B

  def invert(v: B): A
}
