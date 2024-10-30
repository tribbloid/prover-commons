package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.refl.Reflection
import ai.acyclic.prover.commons.viz.TypeIROutput
import ai.acyclic.prover.commons.viz.format.Formats0.KindName

import scala.annotation.StaticAnnotation

trait FormatOvrd extends StaticAnnotation

object FormatOvrd {

  trait SingletonName[T] extends FormatOvrd
  case object SingletonName extends TypeFormat {

    def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { ff =>
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

    object Format extends (TypeFormat => Format)
    case class Format(base: TypeFormat) extends TypeFormat {

      override def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { tt =>
        val byBase = tt.formattedBy(base)
        val result: TypeIROutput = byBase.text <:^ Seq(byBase)
        result
      }
    }
  }

  trait Infix[A, B] extends (A ~~ KindName[this.type] ~~ B)
}
