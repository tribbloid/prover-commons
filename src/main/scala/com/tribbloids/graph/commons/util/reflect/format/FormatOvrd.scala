package com.tribbloids.graph.commons.util.reflect.format

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
}
