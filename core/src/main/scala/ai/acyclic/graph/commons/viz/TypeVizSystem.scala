package ai.acyclic.graph.commons.viz

import ai.acyclic.graph.commons.reflect.format.TypeFormat
import ai.acyclic.graph.commons.reflect.{HasUniverse, Reflection}

trait TypeVizSystem extends HasUniverse {

  val reflection: Reflection
  final val universe: reflection.universe.type = reflection.universe

  val format: TypeVizFormat

  protected class WithFormat(val format: TypeFormat) extends WithFormat.Like[WeakTypeTag] {

    object Strong extends WithFormat.Like[TypeTag] {
      override val format: TypeFormat = WithFormat.this.format
    }
  }

  protected object WithFormat {

    trait Like[F[T] <: WeakTypeTag[T]] {

      def format: TypeFormat
    }
  }
}
