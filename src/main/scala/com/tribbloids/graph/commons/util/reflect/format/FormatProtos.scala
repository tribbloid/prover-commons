package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection

object FormatProtos {

  trait MapOver extends TypeFormat {

    def base: TypeFormat

    def typeMap(refl: Reflection): refl.Type => refl.Type

    final def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val mapFn = typeMap(refl)

      val mapped = ff.typeView.copy(self = ff.typeView.self.map(mapFn))

      ff -> mapped.formattedBy(base)
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

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
      val previous = bases.map { base =>
        ff.formattedBy(base)
      }

      previous.map(_.text).distinct.mkString(" ≅ ") -> previous
    }
  }

  case class Trials(
      bases: TypeFormat*
  ) extends TypeFormat {

    def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
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

      ff -> trials.head
    }
  }

  object Hide {

    case class Package(
        base: TypeFormat
    ) extends TransformText.CanRecursively {

      def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
        type Formatting = refl.Formatting

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

        original.equivalent
          .map { ee =>
            if (ee.typeView != ff.typeView) {
              val newEE = ee.formattedBy(this)
              ff -> newEE: Output
            } else {
              resolve -> ee.parts: Output
            }
          }
          .getOrElse {
            resolve -> original.parts: Output
          }
      }
    }

    case class Static(
        base: TypeFormat
    ) extends TransformText.CanRecursively {

      def resolve(refl: Reflection): refl.Formatting => Output = { ff =>
        type Formatting = refl.Formatting

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

        original.equivalent
          .map { ee =>
            if (ee.typeView != ff.typeView) {
              val newEE = ee.formattedBy(this)
              ff -> newEE: Output
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

  case class TransformText(
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

      var replaced: String = afterOut.text

      def doReplace(from: String, to: String): Unit = {

        replaced = replaced.replaceFirst(s"\\Q$from\\E", to) // TODO: too slow! switch to aho-corasick
      }

      val parts = afterOut.parts
      val transformedParts = parts.map { part =>
        val result = part.formattedBy(this)

        val from = part.text
        val to = result.text

        doReplace(from, to)

        result
      }

      Option(replaced).getOrElse(afterOut.text) -> transformedParts
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

  object TransformText {

    trait CanRecursively extends TypeFormat {

      lazy val recursively: TransformText = TransformText(this)
    }
  }
}
