package com.tribbloids.graph.commons.util.reflect

import scala.reflect.macros.Universe

trait Reflection extends TypeViews {}

object Reflection {

  object Runtime extends Reflection {

    final val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

    // TODO: useless? what's the difference
    // Since we are creating a runtime mirror using the class loader of current thread,
    // we need to use def at here. So, every time we call mirror, it is using the
    // class loader of the current thread.
    override def mirror: universe.Mirror = {

      val result: universe.Mirror = universe
        .runtimeMirror(Thread.currentThread().getContextClassLoader)

      result
    }
  }

  case class CompileTime[U <: Universe](universe: U) extends Reflection {}
}
