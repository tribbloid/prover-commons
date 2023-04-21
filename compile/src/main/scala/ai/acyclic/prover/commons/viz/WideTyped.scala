package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.reflect.ScalaReflection

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
  ) = TypeViz.apply[TT]
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
