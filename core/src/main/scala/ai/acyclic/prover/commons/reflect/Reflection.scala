package ai.acyclic.prover.commons.reflect

import scala.reflect.macros

trait Reflection extends SymbolViewMixin with TypeViewMixin with TypeIRMixin {

  lazy val rootPackageSymbol: universe.Symbol = {

    val tt = universe.typeOf[Reflection.type]
    val sv = symbolView(tt.typeSymbol)

    sv.Owners.internal.leftOpt.get
  }
}

object Reflection {

  object Runtime extends Reflection with HasUniverse.Runtime {}

  case class CompileTime[U <: macros.Universe with Singleton](_universe: U) extends Reflection {

    final override lazy val universe = _universe
  }

//  class General[U <: api.Universe with Singleton](val universe: U) extends Reflection {}
//
//  def General(universe: api.Universe with Singleton) = new General[universe.type](universe)
}
