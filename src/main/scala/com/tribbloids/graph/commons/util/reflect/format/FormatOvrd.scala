package com.tribbloids.graph.commons.util.reflect.format

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait Singleton[T] extends FormatOvrd
  case object Singleton extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val u = ff.refl.getUniverse
      val tt = ff.typeView.self.asInstanceOf[u.Type].dealias

      tt match {
        case v: u.ConstantType =>
          v.value.value.toString
        case v: u.SingletonType =>
          v.toString
        case _ =>
          unsupported(ff)
      }
    }
  }

  trait ~~[A, B] extends FormatOvrd
  case object ~~ extends TypeFormat {

    override def resolve(ff: Formatting): Output = {

      ff.output
    }
  }
}
