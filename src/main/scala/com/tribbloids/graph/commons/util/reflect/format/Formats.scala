package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection

object Formats {

  trait TypeInfo[T] extends FormatOvrd
  case object TypeInfo extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val self = ff.typeView.self.toString

      val parts = ff.typeView.parts

      self -> parts.map { arg =>
        arg.formattedBy(this)
      }
    }
  }

  trait TypeImpl[T] extends FormatOvrd
  case object TypeImpl extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val tt: Reflection#Type = ff.typeView.self
      tt.toString + ": " + tt.getClass.getSimpleName
    }
  }

  trait KindName[T] extends FormatOvrd
  case object KindName extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      ff.typeView.self.typeConstructor.toString
    }
  }

  trait ClassName[T]
  case object ClassName extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      ff.typeView.self.typeSymbol.asClass.fullName
    }
  }
}
