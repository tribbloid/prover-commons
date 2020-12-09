package com.tribbloids.graph.commons.util

import com.tribbloids.graph.commons.util.viz.VizType

import scala.language.implicitConversions

/**
  * @param value use compiler's type inference feature to discover the non-singleton type of the value
  * @tparam T the wide type
  */
case class WideTyped[T](value: T) {

  type Wide = T

  def viz(implicit ttag: TypeTag[T]): VizType = VizType[T]
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
