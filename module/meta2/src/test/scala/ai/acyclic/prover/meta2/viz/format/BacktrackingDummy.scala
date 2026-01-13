package ai.acyclic.prover.meta2.viz.format

import ai.acyclic.prover.meta2.refl.Reflection
import ai.acyclic.prover.meta2.viz.{format, TypeIROutput}
import ai.acyclic.prover.meta2.viz.format.{Backtracking, TypeFormat}

case object BacktrackingDummy extends TypeFormat {

  override def resolve(refl: Reflection): refl.TypeView => TypeIROutput = { _ =>
    throw new Backtracking("backtracking dummy!")
  }
}
