package ai.acyclic.prover.commons.refl

import ai.acyclic.prover.commons.meta.HasITyper

trait HasReflection extends HasITyper {

  override val reflection: Reflection
}
