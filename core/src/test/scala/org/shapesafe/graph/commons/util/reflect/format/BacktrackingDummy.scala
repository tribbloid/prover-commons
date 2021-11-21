package org.shapesafe.graph.commons.util.reflect.format
import org.shapesafe.graph.commons.util.reflect.Reflection

case object BacktrackingDummy extends TypeFormat {

  override def resolve(refl: Reflection): refl.TypeView => IROutput = { v =>
    throw new Backtracking("backtracking dummy!")
  }
}
