package ai.acyclic.prover.commons.reflect.format

import ai.acyclic.prover.commons.reflect.Reflection

object Formats0 {

  trait TypeInfo[T] extends FormatOvrd
  case object TypeInfo extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      val self = tt.self.toString

      val genArgs = tt.genArgs

      self <:% genArgs.map { part =>
        part.formattedBy(this)
      }
    }
  }

  trait TypeImpl[T] extends FormatOvrd
  case object TypeImpl extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      val t: Reflection#Type = tt.self
      t.toString + ": " + t.getClass.getSimpleName
    }
  }

  trait KindName[T] extends FormatOvrd
  case object KindName extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      tt.self.typeConstructor.toString
    }
  }

  trait ClassName[T]
  case object ClassName extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      tt.self.typeSymbol.asClass.fullName
    }
  }
}
