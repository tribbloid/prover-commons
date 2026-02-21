package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.DepDomains
import ai.acyclic.prover.commons.jit.Rule

trait IntermediateRepresentation extends DepDomains with ComputationGraph with Product with Serializable {

  def apply(arg: In): OutK[arg.type]

  type Rules <: Rule
  // used to deduce if this function is compatible with Linear or Affine calculus
}
