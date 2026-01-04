package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.meta.ITyper
import ai.acyclic.prover.commons.refl.Reflection
import ai.acyclic.prover.commons.viz.TypeIROutput

object Formats0 {

  trait TypeInfo[T] extends FormatOvrd
  case object TypeInfo extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => TypeIROutput = { tt =>
      val self = tt.toString

      val genArgs = tt.genArgs

      self <:% genArgs.map { part =>
        part.formattedBy(this)
      }
    }
  }

  trait TypeImpl[T] extends FormatOvrd
  case object TypeImpl extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => TypeIROutput = { tt =>
      val t: ITyper#Type = tt
      t.toString + ": " + t.getClass.getSimpleName
    }
  }

  trait KindName[T] extends FormatOvrd
  case object KindName extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => TypeIROutput = { tt =>
      tt.typeConstructor.toString
    }
  }

  trait ClassName[T]
  case object ClassName extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => TypeIROutput = { tt =>
      tt.typeSymbol.asClass.fullName
    }
  }
}
