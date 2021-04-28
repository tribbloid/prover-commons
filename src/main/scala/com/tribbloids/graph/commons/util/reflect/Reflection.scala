package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.TreeLike
import com.tribbloids.graph.commons.util.reflect.format.{Output, TypeFormat}

import scala.reflect.{api, macros}

trait Reflection extends TypeViews {

  case class Formatting(
      typeView: TypeView,
      format: TypeFormat
  ) extends TreeLike {

    val refl: Reflection = Reflection.this

    lazy val output: Output = format.resolve(this)

    def text: String = output.text
    override lazy val children: Seq[Formatting] = output.causes.collect {
      case v: Formatting => v
    }

    def selfAndChildren: Seq[Formatting] = Seq(this) ++ children

    override def nodeString: String = text

    final def formattedBy(ff: TypeFormat): Formatting = {

      copy(format = ff)
    }

    def fullText: String = text
  }

  object Formatting {}

}

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

  case class CompileTime[U <: macros.Universe](universe: U) extends Reflection {}

  class General[U <: api.Universe](val universe: U) extends Reflection {}

  def General(universe: api.Universe) = new General[universe.type](universe)
}
