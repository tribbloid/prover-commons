package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.{TypeTag, WeakTypeTag}
import com.tribbloids.graph.commons.util.reflect.TypeFormat

class WithFormat(format: TypeFormat) {

  object Strong {

    def apply[T](
        implicit
        ev: TypeTag[T]
    ): VizType = new VizType(ev.tpe, format)

    def infer[T](v: T)(
        implicit
        ev: TypeTag[T]
    ): TermAndTypeOf[T] = new TermAndTypeOf[T](v, ev.tpe, format)
  }

  def apply[T](
      implicit
      ev: WeakTypeTag[T]
  ): VizType = {
    new VizType(ev.tpe, format)
  }

  def infer[T](v: T)(
      implicit
      ev: WeakTypeTag[T]
  ): TermAndTypeOf[T] = new TermAndTypeOf[T](v, ev.tpe, format)
}
