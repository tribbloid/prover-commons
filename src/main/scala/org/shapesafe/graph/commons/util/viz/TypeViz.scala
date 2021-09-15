package org.shapesafe.graph.commons.util.viz

import org.shapesafe.graph.commons.util.reflect.{Reflection, ScalaReflection}

class TypeViz[R <: Reflection](
    override val reflection: R,
    override val format: TypeVizFormat
) extends TypeVizLike {

  override type TTag[T] = universe.WeakTypeTag[T]

  object Strong extends TypeVizLike {

    override type TTag[T] = TypeTag[T]
    override val reflection: Reflection = ScalaReflection

    override val format: TypeVizFormat = TypeViz.this.format
  }

  def withFormat(format: TypeVizFormat = TypeVizFormat.Default) = new TypeViz[R](reflection, format)
}

object TypeViz extends TypeViz(ScalaReflection, TypeVizFormat.Default) {

  def apply[R <: Reflection](reflection: R) = new TypeViz[R](reflection, TypeVizFormat.Default)
}
