package ai.acyclic.prover.meta2

import ai.acyclic.prover.meta2.refl.Reflection

package object meta {

  val ROOT = "<root>"

  val builtInPackageNames: Set[String] = {

    Set(
      "scala",
      "java",
      "java.lang"
    )
  }

  val ScalaReflection: Reflection.Runtime.type = Reflection.Runtime
//  val MacroReflection: Reflection.CompileTime.type = Reflection.CompileTime

  type RuntimeUniverse = scala.reflect.runtime.universe.type
  val RuntimeUniverse: RuntimeUniverse = scala.reflect.runtime.universe

  type WeakTypeTag[T] = RuntimeUniverse.WeakTypeTag[T]
  val WeakTypeTag: RuntimeUniverse.WeakTypeTag.type = RuntimeUniverse.WeakTypeTag
}
