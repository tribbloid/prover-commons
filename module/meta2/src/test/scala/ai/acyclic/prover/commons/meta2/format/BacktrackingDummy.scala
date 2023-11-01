package ai.acyclic.prover.commons.meta2.format

import ai.acyclic.prover.commons.meta2.Reflection

case object BacktrackingDummy extends TypeFormat {

  override def resolve(refl: Reflection): refl.TypeView => IROutput = { _ =>
    throw new Backtracking("backtracking dummy!")
  }
}
