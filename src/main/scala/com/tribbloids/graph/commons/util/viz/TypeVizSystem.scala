package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.reflect.{HasUniverse, Reflection, TypeFormat}

trait TypeVizSystem extends HasUniverse {

  val reflection: Reflection
  final val universe: reflection.universe.type = reflection.universe

  val format: TypeFormat

  class WithFormat(val format: TypeFormat) extends WithFormat.Like[WeakTypeTag] {

    object Strong extends WithFormat.Like[TypeTag] {
      override val format: TypeFormat = WithFormat.this.format
    }
  }

  object WithFormat {

    trait Like[F[T] <: WeakTypeTag[T]] {

      def format: TypeFormat
    }
  }
}
