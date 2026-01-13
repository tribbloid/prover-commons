package ai.acyclic.prover.meta2.refl

import ai.acyclic.prover.meta2.meta.HasITyper

trait HasReflection extends HasITyper {

  override val reflection: Reflection
}
