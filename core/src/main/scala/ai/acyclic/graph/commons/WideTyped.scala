package ai.acyclic.graph.commons

import ai.acyclic.graph.commons.reflect.ScalaReflection
import ai.acyclic.graph.commons.viz.TypeViz

import scala.language.implicitConversions

/**
  * @param value
  *   use compiler's type inference feature to discover the non-singleton type of the value
  * @tparam TT
  *   the wide type
  */
case class WideTyped[TT](value: TT) {

  type Wide = TT

  def viz(
      implicit
      ttag: ScalaReflection.TypeTag[TT]
  ) = TypeViz[TT]
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
