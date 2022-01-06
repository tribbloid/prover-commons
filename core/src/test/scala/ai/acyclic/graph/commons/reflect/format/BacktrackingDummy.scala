package ai.acyclic.graph.commons.reflect.format

import ai.acyclic.graph.commons.reflect.Reflection

case object BacktrackingDummy extends TypeFormat {

  override def resolve(refl: Reflection): refl.TypeView => IROutput = { v =>
    throw new Backtracking("backtracking dummy!")
  }
}
