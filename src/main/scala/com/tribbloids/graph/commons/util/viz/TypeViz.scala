package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.reflect.{Reflection, ScalaReflection, TypeFormat}

class TypeViz[R <: Reflection](
    override val reflection: R,
    override val format: TypeFormat
) extends TypeVizLike {

  override type F[T] = universe.WeakTypeTag[T]

  object Strong extends TypeVizLike {

    override type F[T] = TypeTag[T]
    override val reflection: Reflection = ScalaReflection

    override val format: TypeFormat = TypeViz.this.format
  }

  def withFormat(format: TypeFormat = TypeFormat.Default) = new TypeViz[R](reflection, format)
}

object TypeViz extends TypeViz(ScalaReflection, TypeFormat.Default) {

  def apply[R <: Reflection](reflection: R) = new TypeViz[R](reflection, TypeFormat.Default)
}
