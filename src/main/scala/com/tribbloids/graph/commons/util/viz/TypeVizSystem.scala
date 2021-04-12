package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.reflect.format.TypeFormat
import com.tribbloids.graph.commons.util.reflect.{HasUniverse, Reflection}

trait TypeVizSystem extends HasUniverse {

  val reflection: Reflection
  final val _universe: reflection._universe.type = reflection._universe

  val format: TypeVizFormat

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
