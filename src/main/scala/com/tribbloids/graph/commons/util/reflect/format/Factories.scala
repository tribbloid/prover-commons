package com.tribbloids.graph.commons.util.reflect.format

object Factories {
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

  case class HidePackages(
      base: TypeFormat
  ) extends TypeFormat {
    override def resolve(ff: Formatting): Output = {

      val prev = ff.formattedBy(base)

      var out = prev.nodeString

      val symbols = prev.selfAndChildren.flatMap { v =>
        v.typeView.Recursive.collectSymbols
      }

      for (ss <- symbols) {

        out = out.replaceAll(ss.packagePrefix, "")
      }

      out -> Seq(prev)
    }
  }
}
