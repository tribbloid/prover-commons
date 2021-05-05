package com.tribbloids.graph.commons.util.reflect.format

object DerivedFormats {

  case class DeAlias(
      base: TypeFormat
  ) extends TypeFormat {
    override def resolve(ff: Formatting): Output = {

      val refl = ff.refl
      val _ff = ff.asInstanceOf[refl.Formatting]

      val v = _ff.typeView

      val vN = v.copy(self = v.deAlias)
      val ffN = _ff.copy(typeView = vN)

      ffN.formattedBy(base)
    }
  }

  case class Concat(
      bases: TypeFormat*
  ) extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val prevs = bases.map { base =>
        ff.formattedBy(base)
      }

      prevs.map(_.nodeString).distinct.mkString(" ≅ ") -> prevs
    }
  }

  case class HidePackage(
      base: TypeFormat
  ) extends TypeFormat {

    override def resolve(ff: Formatting): Output = {

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

    override def resolve(ff: Formatting): Output = {

      val refl = ff.refl
      val _ff = ff.asInstanceOf[refl.Formatting]

      val parts = _ff.parts

      val transformedParts = parts.map { part =>
        val result = _ff.copy(typeView = part)

        val before = part.text
        val after = result.text

        (result, before, after)
      }

      var finalText = _ff.text

      transformedParts.foreach {
        case (r, b, a) =>

      }
    }
  }

  object TransformUp {}

  def HidePackages(base: TypeFormat) = TransformUp(base, HidePackage(base))
}
