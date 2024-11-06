package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.refl.Reflection
import ai.acyclic.prover.commons.viz.TypeIROutput
import ai.acyclic.prover.commons.viz.format.TypeFormat

case object BacktrackingDummy extends TypeFormat {

  override def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { _ =>
    throw new Backtracking("backtracking dummy!")
  }
}
