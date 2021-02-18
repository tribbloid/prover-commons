package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.ScalaReflection.{TypeTag, WeakTypeTag}
import com.tribbloids.graph.commons.util.TreeFormat
import com.tribbloids.graph.commons.util.viz.VizType

case class TypeFormat(
    showArgTree: Boolean = true,
    hidePackages: Boolean = false,
    hideAlias: Boolean = false,
    treeFormat: TreeFormat = TreeFormat.Indent2
) {

  class WithFormat {

    object Strong {

      def apply[T](
          implicit
          ev: TypeTag[T]
      ): VizType = VizType(ev.tpe, TypeFormat.this)

      def infer[T](v: T)(
          implicit
          ev: TypeTag[T]
      ): VizType = apply[T]
    }

    def apply[T](
        implicit
        ev: WeakTypeTag[T]
    ): VizType = {
      VizType(ev.tpe, TypeFormat.this)
    }

    def infer[T](v: T)(
        implicit
        ev: WeakTypeTag[T]
    ): VizType = apply[T]
  }
}

object TypeFormat {

  object Default extends TypeFormat()
}
