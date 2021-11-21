package org.shapesafe.graph.commons.util.reflect.format

import org.shapesafe.graph.commons.util.reflect.Reflection
import org.shapesafe.graph.commons.util.reflect.format.Formats0.KindName

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait Only[T] extends FormatOvrd
  case object Only extends TypeFormat {

    def resolve(refl: Reflection): refl.TypeView => IROutput = { ff =>
      try {
        ff.getOnlyInstance.toString
      } catch {
        case _: UnsupportedOperationException =>
          backtrack(ff)
      }
    }
  }

  trait ~~[A, B] extends FormatOvrd
  case object ~~ extends TypeFormat.Constructor {

    case class Format(base: TypeFormat) extends TypeFormat {
      override def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
        val byBase = tt.formattedBy(base)
        byBase.text <:^ Seq(byBase)
      }
    }
  }

  trait Infix[A, B] extends (A ~~ KindName[this.type] ~~ B)
}
