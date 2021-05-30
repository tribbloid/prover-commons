package org.shapesafe.graph.commons.util.viz

import org.shapesafe.graph.commons.util.reflect.format.TypeFormat
import org.shapesafe.graph.commons.util.reflect.{HasUniverse, Reflection}
import org.shapesafe.graph.commons.util.reflect.Reflection
import org.shapesafe.graph.commons.util.reflect.format.TypeFormat

trait TypeVizSystem extends HasUniverse {

  val reflection: Reflection
  final val universe: reflection.universe.type = reflection.universe

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
