package org.shapesafe.graph.commons.util.reflect.format

import org.shapesafe.graph.commons.util.reflect.Reflection

object Formats1 { //higher-order format constructors

  trait MapOver extends TypeFormat {

    def base: TypeFormat

    def typeMap(refl: Reflection): refl.Type => refl.Type

    final def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
      val mapFn = typeMap(refl)

      val mapped = ff.typeView.copy(self = ff.typeView.self.map(mapFn))

      ff.withCanonical(mapped.formattedBy(base))
    }
  }

  case class DeAlias(base: TypeFormat) extends MapOver {

    override def typeMap(refl: Reflection): refl.Type => refl.Type = { tt =>
      tt.dealias
    }
  }

  case class Concat(
      bases: TypeFormat*
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
      val types = bases.map { base =>
        ff.formattedBy(base)
      }

      val texts = types.map(_.text)

      texts.distinct.mkString(" ≅ ") -> types
    }
  }

  case class Trials(
      bases: TypeFormat*
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
      val trials = bases
        .to(LazyList)
        .flatMap { base: TypeFormat =>
          try {
            val result = ff.formattedBy(base)
            Some(result)
          } catch {
            case _: Backtracking =>
              None
          }
        }

      ff.withCanonical(trials.head)
    }
  }

  object Hide {

    case class HidePackage(
        base: TypeFormat
    ) extends RecursiveForm.HasRecursiveForm {

      def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
        type Formatting = refl.FormattedType

        val original = ff.formattedBy(base)

        def resolve: String = {

          val full = original.text

          val constructor = original.typeView.constructor

          val shorten = if (full.startsWith(constructor.canonicalName)) {
            constructor.Prefixes.packages.simpleName + full.stripPrefix(constructor.canonicalName)
          } else {
            full
          }

          shorten
        }

        original.simplified
          .map { ee =>
            if (ee.typeView != ff.typeView) {
              val newEE = ee.formattedBy(this)
              ff.withCanonical(newEE)
            } else {
              resolve -> ee.parts: Output
            }
          }
          .getOrElse {
            resolve -> original.parts: Output
          }
      }
    }

    case class HideStatic(
        base: TypeFormat
    ) extends RecursiveForm.HasRecursiveForm {

      def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
        type Formatting = refl.FormattedType

        val original = ff.formattedBy(base)

        def resolve: String = {

          val full = original.text

          val constructor = original.typeView.constructor

          val shorten = if (full.startsWith(constructor.canonicalName)) {
            constructor.Prefixes.static.simpleName + full.stripPrefix(constructor.canonicalName)
          } else {
            full
          }

          shorten
        }

        original.simplified
          .map { ee =>
            if (ee.typeView != ff.typeView) {
              val newEE = ee.formattedBy(this)
              ff.withCanonical(newEE)
            } else {
              resolve -> ee.parts: Output
            }
          }
          .getOrElse {
            resolve -> original.parts: Output
          }
      }
    }
  }

//  case class TransformText(
//      after: TypeFormat
//  ) extends MapOver {
//
//    def base = TypeInfo
//
//    override def typeMap(refl: Reflection): refl.Type => refl.Type = { tt =>
//      val transformed =
//        try {
//          val cc = tt.typeConstructor
//          refl.TypeView(cc).formattedBy(base).text
//        } catch {
//          case e: Throwable =>
//            refl.TypeView(tt).formattedBy(base).text
//        }
//
//      val internal = refl.universe.internal
//
//      internal.newFreeType("Abc")
//
//      val fakeName = refl.universe.TypeName(transformed + "abc")
//      val fakeSymbol = internal.newTypeSymbol(refl.rootPackageSymbol, fakeName)
//
//      fakeSymbol.toString
//
//      val result = internal.typeRef(refl.universe.NoPrefix, fakeSymbol, tt.typeArgs)
//      result
//    }
//  }

  case class RecursiveForm(
      after: TypeFormat
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.FormattedType => Output = { ff =>
      val afterOut = ff.formattedBy(after)

      var replaced: String = afterOut.text

      def doReplace(from: String, to: String): Unit = {

        replaced = replaced.replaceFirst(s"\\Q$from\\E", to) // TODO: regex is too slow! switch to aho-corasick
      }

      val parts = afterOut.parts
      val transformedParts = parts.map { part =>
        val result = part.formattedBy(this)

        val from = part.text
        val to = result.text

        doReplace(from, to)

        result
      }

      Output(
        Option(replaced).getOrElse(afterOut.text),
        transformedParts,
        afterOut.simplified
      )
    }
  }

  object RecursiveForm {

    trait HasRecursiveForm extends TypeFormat {

      lazy val recursively: RecursiveForm = RecursiveForm(this)
    }
  }
}
