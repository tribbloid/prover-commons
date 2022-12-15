package ai.acyclic.prover.commons.reflect.format

import ai.acyclic.prover.commons.reflect.Reflection

import java.util.regex.Matcher

object Formats1 { // higher-order format constructors

  trait HasBase extends TypeFormat {

    def base: TypeFormat
  }

  trait MapBefore extends HasBase {

    def before(refl: Reflection): refl.Type => refl.Type

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      val beforeFn = before(refl)

      val oldT = tt.self
      val transformRoot = beforeFn(oldT)
      val transformBranches = transformRoot.map(beforeFn)

      val mapped = tt.copy(self = transformBranches)

      val mapped_formatted = mapped.formattedBy(base)

      mapped_formatted.text <:^ Seq(mapped_formatted)
    }
  }

  /**
    * T -> DeAlias +> base
    */
  case class DeAlias(base: TypeFormat) extends MapBefore {

    override def before(refl: Reflection): refl.Type => refl.Type = { tt =>
      tt.dealias
    }
  }

  /**
    * T -> base1
    * -> base2
    * -> ... +> concat
    */
  case class Concat(
      bases: TypeFormat*
  ) extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      val byBases = bases.map { base =>
        tt.formattedBy(base)
      }

      val texts = byBases.map(_.text)

      texts.distinct.mkString(" ≅ ") <:^ byBases
    }
  }

  case class Trials(
      bases: TypeFormat*
  ) extends TypeFormat {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      val trials = bases
        .to(LazyList)
        .flatMap { base: TypeFormat =>
          try {
            val result = tt.formattedBy(base)
            Some(result)
          } catch {
            case _: Backtracking =>
              None
          }
        }

      val chosen = trials.head

      chosen.text <:^ Seq(chosen)
    }
  }

  trait UseDelegate extends HasBase {}

  object Hide {

    /**
      * T -> base +> HidePackage
      */
    case class HidePackage(
        base: TypeFormat
    ) extends RecursiveForm.HasRecursiveForm {

      override def constructor: TypeFormat => TypeFormat = Hide.HidePackage

      final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
        val byBase = tt.formattedBy(base)

        val full = byBase.text

        val constructor = byBase.typeView.constructor

        val shorten = if (full.startsWith(constructor.canonicalName)) {

          val cPackage = constructor.Prefixes.packages.simpleName

          cPackage + full.stripPrefix(constructor.canonicalName)
        } else {
          full
        }

        shorten <:^ Seq(byBase)
      }
    }

    /**
      * T -> base +> HideStatic
      */
    case class HideStatic(
        base: TypeFormat
    ) extends RecursiveForm.HasRecursiveForm {

      override def constructor: TypeFormat => TypeFormat = Hide.HideStatic

      final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
        type Formatting = refl.TypeIR

        val byBase = tt.formattedBy(base)

        val full = byBase.text

        val constructor = byBase.typeView.constructor

        val shorten = if (full.startsWith(constructor.canonicalName)) {
          constructor.Prefixes.static.simpleName + full.stripPrefix(constructor.canonicalName)
        } else {
          full
        }

        shorten <:^ Seq(byBase)
      }
    }
  }

  trait TransformDown extends HasBase {}

  /**
    * T -> base +> (T#Args -> base) +> replace
    */
  case class RecursiveForm(
      base: TypeFormat,
      transformer: TypeFormat => TypeFormat
  ) extends TransformDown {

    final def resolve(refl: Reflection): refl.TypeView => IROutput = { tt =>
      val transformedBase = transformer(base)

      val byBase = tt.formattedBy(transformedBase)

      var replacedText: String = byBase.text

      def doReplace(from: String, to: String): Unit = {

        replacedText = replacedText.replaceFirst(
          s"\\Q$from\\E",
          Matcher.quoteReplacement(to)
        ) // TODO: regex is too slow! switch to aho-corasick
      }

      val annotations = byBase.annotations
      val transformedAnnotations = annotations.map { part =>
        {
          val sanity = part.derivedFrom.isEmpty
          require(
            sanity,
            s"${this.getClass.getSimpleName} cannot be applied to derived format"
          )
        }

        val subFormat = RecursiveForm(part.format, transformer)

        val result = part.formattedBy(subFormat)

        val from = part.text
        val to = result.text

        doReplace(from, to)

        result
      }

      val result = IROutput(
        replacedText,
        transformedAnnotations,
        derivedFrom = Seq(byBase)
      )
      result
    }
  }

  object RecursiveForm {

    trait HasRecursiveForm extends HasBase {

      def constructor: TypeFormat => TypeFormat

      lazy val recursively: RecursiveForm = RecursiveForm(base, constructor)
    }
  }
}
