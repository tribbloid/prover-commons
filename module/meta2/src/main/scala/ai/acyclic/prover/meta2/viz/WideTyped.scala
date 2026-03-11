package ai.acyclic.prover.meta2.viz

import ai.acyclic.prover.meta2.refl.Reflection
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
      ttag: Reflection.Runtime.TypeTag[TT]
  ) = TypeViz.apply[TT]
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
