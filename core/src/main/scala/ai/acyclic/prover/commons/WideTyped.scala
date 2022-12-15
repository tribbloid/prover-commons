package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.reflect.ScalaReflection
import ai.acyclic.prover.commons.viz.TypeViz

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
