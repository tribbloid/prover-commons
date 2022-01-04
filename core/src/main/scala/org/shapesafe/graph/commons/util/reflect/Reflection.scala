package org.shapesafe.graph.commons.util.reflect

import scala.reflect.{api, macros}

trait Reflection extends SymbolViews with TypeViews with TypeIRs {

  lazy val rootPackageSymbol: universe.Symbol = {

    val tt = universe.typeOf[Reflection.type]
    val sv = symbolView(tt.typeSymbol)

    sv.Owners.internal.leftOpt.get
  }
}

object Reflection {

  object Runtime extends Reflection {

    final val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  }

  case class CompileTime[U <: macros.Universe](universe: U) extends Reflection {}

  class General[U <: api.Universe](val universe: U) extends Reflection {}

  def General(universe: api.Universe) = new General[universe.type](universe)
}
