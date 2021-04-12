package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.TreeLike
import com.tribbloids.graph.commons.util.reflect.format.TypeFormat

import scala.reflect.{api, macros}

trait Reflection extends TypeViews {

  case class Formatting(
      typeView: TypeView,
      format: TypeFormat
  ) extends TreeLike {

    val refl: Reflection = Reflection.this

    lazy val resolved: TypeFormat.Output = format.resolve(this)

    def text: String = resolved.text
    override lazy val children: Seq[Formatting] = resolved.children.collect {
      case v: Formatting => v
    }

    def selfAndChildren: Seq[Formatting] = Seq(this) ++ children

    override def nodeString: String = text

    final def formattedBy(ff: TypeFormat): Formatting = {

      copy(format = ff)
    }

    def complete: String = text
  }

  object Formatting {}

}

object Reflection {

  object Runtime extends Reflection {

    final val _universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

    // TODO: useless? what's the difference
    // Since we are creating a runtime mirror using the class loader of current thread,
    // we need to use def at here. So, every time we call mirror, it is using the
    // class loader of the current thread.
    override def mirror: _universe.Mirror = {

      val result: _universe.Mirror = _universe
        .runtimeMirror(Thread.currentThread().getContextClassLoader)

      result
    }
  }

  case class CompileTime[U <: macros.Universe](_universe: U) extends Reflection {}

  class General[U <: api.Universe](val _universe: U) extends Reflection {}

  def General(universe: api.Universe) = new General[universe.type](universe)
}
