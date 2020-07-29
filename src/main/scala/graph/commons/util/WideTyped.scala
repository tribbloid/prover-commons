package graph.commons.util

import scala.language.implicitConversions

case class WideTyped[_T](value: _T) {

  type WideType = _T
}

object WideTyped {

  implicit def unbox[T](v: WideTyped[T]): T = v.value
}
