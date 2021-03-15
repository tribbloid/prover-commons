package com.tribbloids.graph.commons.util.reflect

import scala.reflect.api.Universe

trait HasUniverse {

  val universe: Universe

  final type UU = universe.type
  final def getUniverse: UU = universe

  type TypeTag[T] = universe.TypeTag[T]
  type WeakTypeTag[T] = universe.WeakTypeTag[T]
  type Type = universe.Type

  def rootMirror: universe.Mirror = universe.rootMirror

  def mirror: universe.Mirror = rootMirror
}
