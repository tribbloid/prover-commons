package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.IDMixin
import com.tribbloids.graph.commons.util.ScalaReflection._

case class TypeID(
    _type: universe.Type
) extends IDMixin {

  lazy val symbol: universe.Symbol = _type.typeSymbol

  lazy val showStr: String = _type.toString

  override protected def _id: Any = symbol -> showStr
}
