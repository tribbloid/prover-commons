package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.PreDef.:=>

trait FnFixture {

  // Single Abstract Method definition
  lazy val _fn0: Int :=> Int = { v =>
    v + 1
  }

}
