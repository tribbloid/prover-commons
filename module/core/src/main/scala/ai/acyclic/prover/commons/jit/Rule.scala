package ai.acyclic.prover.commons.jit

trait Rule
object Rule {

  sealed trait Affine extends Rule
  sealed trait Linear extends Affine
}
