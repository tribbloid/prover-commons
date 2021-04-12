package com.tribbloids.graph.commons.util.reflect

import scala.reflect.api.Universe

trait HasUniverse {

  val _universe: Universe

  final type UU = _universe.type
  final lazy val universe: UU = _universe

  type TypeTag[T] = _universe.TypeTag[T]
  type WeakTypeTag[T] = _universe.WeakTypeTag[T]
  type Type = _universe.Type

  def rootMirror: _universe.Mirror = _universe.rootMirror

  def mirror: _universe.Mirror = rootMirror
}
