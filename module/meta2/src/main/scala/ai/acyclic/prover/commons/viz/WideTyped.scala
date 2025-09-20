package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.refl.Reflection

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
      ttag: Reflection.Runtime.TypeTag[TT]
  ) = TypeViz.apply[TT]
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
