package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection
import com.tribbloids.graph.commons.util.reflect.format.Formats.KindName

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait Only[T] extends FormatOvrd
  case object Only extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
//      ff.typeView.self.dealias match {
//        case v: refl.universe.ConstantType =>
//          "" + v.value.value
//        case v @ _ =>
//          throw new UnsupportedOperationException(s"$v is not a constant")
//      }

      ff.typeView.getOnlyInstance.toString
    }
  }

  trait ~~[A, B] extends FormatOvrd
  case object ~~ extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      ff.output
    }
  }

  trait Infix[A, B] extends (A ~~ KindName[this.type] ~~ B)

  trait Prefix extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
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
