package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection

object DerivedFormats {

  case class DeAlias(
      base: TypeFormat
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val v = ff.typeView

      val vN = v.copy(self = v.deAlias)
      val ffN = ff.copy(typeView = vN)

      ffN.formattedBy(base)
    }
  }

  case class Concat(
      bases: TypeFormat*
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val previous = bases.map { base =>
        ff.formattedBy(base)
      }

      previous.map(_.text).distinct.mkString(" ≅ ") -> previous
    }
  }

  case class HidePackage(
      base: TypeFormat
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val original = ff.formattedBy(base)
      var out = original.text

      val symbols = original.typeView.Recursive.collectSymbols

      for (ss <- symbols) {

        out = out.stripPrefix(ss.packagePrefix)
      }

      out -> original.children
    }
  }

  case class TransformUp(
      before: TypeFormat,
      after: TypeFormat
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      lazy val beforeOut = ff.formattedBy(before)
      lazy val afterOut = ff.formattedBy(after)

      var swapped: String = null

      def swap(before: String, after: String) = {

        val _swapped = Option(swapped).getOrElse {
          if (afterOut.text.contains(before)) afterOut.text
          else beforeOut.text
        }

        swapped = _swapped.replace(before, after)
      }

      val children = afterOut.children
      val transformedParts = children.map { part =>
        val result = part.formattedBy(this)

        val before = part.text
        val after = result.text

        swap(before, after)

        result
      }

      Option(swapped).getOrElse(afterOut.text) -> transformedParts
    }
  }

  object TransformUp {}

  def HidePackages(base: TypeFormat) = TransformUp(base, HidePackage(base))
}
