package ai.acyclic.prover.commons.reflect.format

import ai.acyclic.prover.commons.reflect.Reflection
import ai.acyclic.prover.commons.reflect.format.Formats0.KindName

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait SingletonName[T] extends FormatOvrd
  case object SingletonName extends TypeFormat {

    def resolve(refl: Reflection): refl.TypeView => IROutput = { ff =>
      try {
        ff.singletonName
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
