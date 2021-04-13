package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection
import com.tribbloids.graph.commons.util.reflect.format.TypeFormat.Output

import scala.language.implicitConversions

trait TypeFormat {

  def +>(fn: TypeFormat => TypeFormat): TypeFormat = fn(this)

  def resolve(ff: Formatting): Output

  def joinText(v: Seq[String]): String = v.mkString(" ")

  def unsupported(): Nothing = {
    throw new UnsupportedOperationException(
      "unsupported"
    )
  }

  // TODO: add loop elimination
  implicit def fromFormatting(v: Formatting): Output = {
    require(v.format != this, "cannot convert Formatting into Output: may trigger dead loop")
    Output(v.text, Seq(v))
  }
}

object TypeFormat {

  case class Output(
      text: String,
      children: Seq[Formatting] = Nil
  )

  object Output {

    implicit def fromText(v: String): Output = Output(v)

    implicit def fromTuple(v: (String, Seq[Formatting])): Output = Output(v._1, v._2)
  }

  val Default: Concat = Concat(
    DeAlias(Type),
    Type
  )

  val Short: HidePackages = HidePackages(DeAlias(Type))

  trait Type[T]
  case object Type extends TypeFormat {
    override def resolve(ff: Formatting): Output =
      ff.typeView.self.toString
  }

  trait TypeInternal[T]
  case object TypeInternal extends TypeFormat {
    override def resolve(ff: Formatting): Output = {
      val tt: Reflection#Type = ff.typeView.self
      tt.toString + ": " + tt.getClass.getSimpleName
    }
  }

  trait Kind[T]
  case object Kind extends TypeFormat {
    override def resolve(ff: Formatting): Output =
      ff.typeView.self.typeConstructor.toString
  }

  trait Class[T]
  case object Class extends TypeFormat {
    override def resolve(ff: Formatting): Output =
      ff.typeView.self.typeSymbol.asClass.fullName
  }

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
