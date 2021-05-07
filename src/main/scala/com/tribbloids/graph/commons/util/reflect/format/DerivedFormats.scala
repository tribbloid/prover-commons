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
//      val beforeOut = ff.formattedBy(before)
      val afterOut = ff.formattedBy(after)

//      val afterParts = afterOut.parts.map(_.text).toList.distinct
//      val part_redact = afterParts.zipWithIndex.map {
//        case (v, i) =>
//          v -> s"?${i}"
//      }
//      val partMap_> = Map(part_redact: _*)
//      val partMap_< = Map(part_redact.map(v => v.swap): _*)

      var swapped: String = afterOut.text

      def swap(from: String, to: String): Unit = {

        swapped = swapped.replaceFirst(s"\\Q$from\\E", to) // TODO: too slow! switch to aho-corasick
      }

      val parts = afterOut.parts
      val transformedParts = parts.map { part =>
        val result = part.formattedBy(this)

        val from = part.text
        val to = result.text

        swap(from, to)

        result
      }

      Option(swapped).getOrElse(afterOut.text) -> transformedParts
    }

//    case class Contextual() {
//
//      lazy val cache = mutable.Map.empty[String, String]
//
//      object TransformImpl extends TypeFormat {
//
//        override def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
//          val afterOut = ff.formattedBy(after)
//
//          val parts = afterOut.parts
//          val transformedParts = parts.map { part =>
//            val result = part.formattedBy(this)
//
//            val from = part.text
//            val to = result.text
//
//            cache += from -> to
//
//            result
//          }
//
//          afterOut.text -> transformedParts
//        }
//      }
//    }
  }

  object TransformUp {}

  def HidePackages(base: TypeFormat): TransformUp = TransformUp(base, HidePackage(base))
}
