package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection

object Formats {

  trait TypeInfo[T] extends FormatOvrd
  case object TypeInfo extends TypeFormat {
    override def resolve(ff: Formatting): Output = {

      val self = ff.typeView.self.toString

      val args = ff.typeView.args

      self -> args.map { arg =>
        arg.formattedBy(this)
      }
    }
  }

  trait TypeImpl[T] extends FormatOvrd
  case object TypeImpl extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val tt: Reflection#Type = ff.typeView.self
      tt.toString + ": " + tt.getClass.getSimpleName
    }
  }

  trait KindName[T] extends FormatOvrd
  case object KindName extends TypeFormat {
    override def resolve(ff: Formatting): Output = {

      ff.typeView.self.typeConstructor.toString
    }
  }

  trait ClassName[T]
  case object ClassName extends TypeFormat {
    override def resolve(ff: Formatting): Output =
      ff.typeView.self.typeSymbol.asClass.fullName
  }
}
