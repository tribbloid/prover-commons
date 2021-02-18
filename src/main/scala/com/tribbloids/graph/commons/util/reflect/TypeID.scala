package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.IDMixin
import com.tribbloids.graph.commons.util.ScalaReflection._

case class TypeID(
    self: universe.Type
) extends IDMixin {

  lazy val symbols: Seq[universe.Symbol] = Seq(
    self.typeSymbol,
    self.termSymbol
  ).filter { ss =>
    ss != universe.NoSymbol
  }

  lazy val showStr: String = self.toString

  override protected def _id: Any = symbols -> showStr
}
