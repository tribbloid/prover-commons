package com.tribbloids.graph.commons.util

import com.tribbloids.graph.commons.util.viz.VizType

import scala.language.implicitConversions

/**
  * @param value use compiler's type inference feature to discover the non-singleton type of the value
  * @tparam TT the wide type
  */
case class WideTyped[TT](value: TT) {

  type Wide = TT

  def viz(implicit ttag: TypeTag[TT]): VizType = VizType[TT]
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
