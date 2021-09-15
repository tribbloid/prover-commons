package org.shapesafe.graph.commons.util.reflect.format

import org.shapesafe.graph.commons.util.reflect.Reflection
import org.shapesafe.graph.commons.util.reflect.format.Formats0.KindName

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait Only[T] extends FormatOvrd
  case object Only extends TypeFormat {

    def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
      try {
        ff.typeView.getOnlyInstance.toString
      } catch {
        case _: UnsupportedOperationException =>
          backtrack(ff)
      }
    }
  }

  trait ~~[A, B] extends FormatOvrd
  case object ~~ extends TypeFormat {

    def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
      ff.output
    }
  }

  trait Infix[A, B] extends (A ~~ KindName[this.type] ~~ B)

  trait Prefix extends TypeFormat {

    def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
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
