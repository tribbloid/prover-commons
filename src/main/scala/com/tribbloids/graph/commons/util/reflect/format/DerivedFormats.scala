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

      val result = ff -> ffN.formattedBy(base)
      result
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
      type Formatting = refl.Formatting

      val original = ff.formattedBy(base)

      def doResolve(): String = {

        val full = original.text

        val constructor = original.typeView.constructor

        val shorten = if (full.startsWith(constructor.fullString)) {
          constructor.shortString + full.stripPrefix(constructor.fullString)
        } else {
          full
        }

        shorten
      }

      original.equivalent
        .map { ee =>
          if (ee.typeView != ff.typeView) {
            val newEE = ee.formattedBy(this)
            ff -> newEE: Output
          } else {
            doResolve -> ee.parts: Output
          }
        }
        .getOrElse {
          doResolve -> original.parts: Output
        }
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

      def swap(from: String, to: String): Unit = {

        val _swapped = Option(swapped).getOrElse {
          if (afterOut.text.contains(from)) afterOut.text
          else beforeOut.text
        }

        swapped = _swapped.replace(from, to)
      }

      val children = afterOut.children
      val transformedParts = children.map { part =>
        val result = part.formattedBy(this)

        val from = part.text
        val to = result.text

        swap(from, to)

        result
      }

      Option(swapped).getOrElse(afterOut.text) -> transformedParts
    }
  }

  object TransformUp {}

  def HidePackages(base: TypeFormat): TransformUp = TransformUp(base, HidePackage(base))
}
