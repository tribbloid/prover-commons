package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.TreeLike
import com.tribbloids.graph.commons.util.reflect.format.{Output, TypeFormat}

import scala.language.implicitConversions
import scala.reflect.{api, macros}

trait Reflection extends TypeViews {

  case class Formatting(
      typeView: TypeView,
      format: TypeFormat
  ) extends TreeLike {

    val refl: Reflection = Reflection.this

    lazy val output: Output = format.resolve(Reflection.this).apply(this)

    def text: String = output.text
    def parts: Seq[Formatting] = {
      val result = output.parts.collect {
        case v: Formatting => v
      }

      result.foreach { v =>
        require(text.contains(v.text), s"Part '${v.text}' cannot be found in whole '$text'")
      }

      result
    }
    def equivalent: Option[Formatting] = output.equivalent.collect {
      case v: Formatting => v
    }

    override lazy val children: Seq[Formatting] = {

      val result = parts ++ equivalent

      result
    }

    def selfAndChildren: Seq[Formatting] = Seq(this) ++ this.children

    override def nodeString: String = text

//    final def formattedBy(ff: TypeFormat): Formatting = {
//
//      copy(format = ff)
//    }
  }

  object Formatting {

    implicit def asTypeView(v: Formatting): TypeView = v.typeView
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
    override def mirror: universe.Mirror = {

      val result: universe.Mirror = universe
        .runtimeMirror(_classloader)

      result
    }
  }

  case class CompileTime[U <: macros.Universe](universe: U) extends Reflection {}

  class General[U <: api.Universe](val universe: U) extends Reflection {}

  def General(universe: api.Universe) = new General[universe.type](universe)
}
