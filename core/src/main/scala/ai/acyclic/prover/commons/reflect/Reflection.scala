package ai.acyclic.prover.commons.reflect

import scala.reflect.{api, macros}

trait Reflection extends SymbolViewMixin with TypeViewMixin with TypeIRMixin {

  lazy val rootPackageSymbol: universe.Symbol = {

    val tt = universe.typeOf[Reflection.type]
    val sv = symbolView(tt.typeSymbol)

    sv.Owners.internal.leftOpt.get
  }
}

object Reflection {

  object Runtime extends Reflection {

    final val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

    val _classloader: ClassLoader = Reflection.getClass.getClassLoader

    // TODO: useless? what's the difference
    // Since we are creating a runtime mirror using the class loader of current thread,
    // we need to use def at here. So, every time we call mirror, it is using the
    // class loader of the current thread.
    override lazy val mirror: universe.Mirror = {

      val result: universe.Mirror = universe
        .runtimeMirror(_classloader)

      result
    }
  }

  case class CompileTime[U <: macros.Universe](universe: U) extends Reflection {}

  class General[U <: api.Universe](val universe: U) extends Reflection {}

  def General(universe: api.Universe) = new General[universe.type](universe)
}
