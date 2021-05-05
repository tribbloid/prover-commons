package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.format.Formats.KindName

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait Only[T] extends FormatOvrd
  case object Only extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val only = ff.typeView.getOnlyInstance

      "" + only
    }
  }

  trait ~~[A, B] extends FormatOvrd
  case object ~~ extends TypeFormat {

    override def resolve(ff: Formatting): Output = {

      ff.output
    }
  }

  trait Infix[A, B] extends (A ~~ KindName[this.type] ~~ B)

  trait Prefix extends TypeFormat {

    override def resolve(ff: Formatting): Output = {

      ff.output
    }

    override def joinText(v: Seq[String]): String = {

      v.head + v.slice(1, Int.MaxValue).mkString("(", ", ", ")")
    }
  }

  trait Prefix1[S, A]
  object Prefix1 extends Prefix

  trait Prefix2[S, A, B]
  object Prefix2 extends Prefix
}
