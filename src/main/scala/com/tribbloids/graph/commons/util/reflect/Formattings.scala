package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.TreeLike
import com.tribbloids.graph.commons.util.reflect.format.{Output, TypeFormat}

import scala.language.implicitConversions

trait Formattings extends HasUniverse {
  self: Reflection =>

  case class Formatting(
      typeView: TypeView,
      format: TypeFormat
  ) extends TreeLike {

    val refl: Reflection = self

    lazy val output: Output = format.resolve(self).apply(this)

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
