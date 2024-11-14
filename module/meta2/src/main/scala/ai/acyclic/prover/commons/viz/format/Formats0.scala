package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.meta.ITyper
import ai.acyclic.prover.commons.refl.Reflection
import ai.acyclic.prover.commons.viz.TypeIROutput

object Formats0 {

  trait TypeInfo[T] extends FormatOvrd
  case object TypeInfo extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { tt =>
      val self = tt.as.toString

      val genArgs = tt.genArgs

      self <:% genArgs.map { part =>
        refl.TypeOps(part).formattedBy(this)
      }
    }
  }

  trait TypeImpl[T] extends FormatOvrd
  case object TypeImpl extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { tt =>
      val t: ITyper#Type = tt.as
      t.toString + ": " + t.getClass.getSimpleName
    }
  }

  trait KindName[T] extends FormatOvrd
  case object KindName extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { tt =>
      tt.as.typeConstructor.toString
    }
  }

  trait ClassName[T]
  case object ClassName extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeOps => TypeIROutput = { tt =>
      tt.as.typeSymbol.asClass.fullName
    }
  }
}
