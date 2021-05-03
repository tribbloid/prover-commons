package com.tribbloids.graph.commons.util.reflect.format

import scala.language.implicitConversions

trait TypeFormat {

  def resolve(ff: Formatting): Output

  def joinText(v: Seq[String]): String = v.mkString(" ")

  def backtrack(ff: Formatting): Nothing = {
    throw new UnsupportedOperationException(
      s"Type ${ff.typeView} is not supported by format $this"
    )
  }

  implicit def fromText(v: String): Output = Output(v)

  implicit def fromTuple(v: (String, Seq[Formatting])): Output = Output(v._1, v._2)

  implicit def fromFormatting(v: Formatting): Output = {
    require(v.format != this, "cannot convert Formatting into Output: may trigger dead loop")
    Output(v.text, Seq(v))
  }

  def ~(factory: TypeFormat => TypeFormat): TypeFormat = factory(this)

  // TODO: the following should be moved into a view that also contains TypeVizFormat
  //  should make compile-time macro much easier to define
  object DeAlias extends Factories.DeAlias(this)

  object HidePackages extends Factories.HidePackages(this)

  object Both
      extends Factories.Concat(
        this.DeAlias,
        this
      )

  object Short
      extends Factories.HidePackages(
        DeAlias
      )
}

object TypeFormat {

  val Default: Formats.TypeInfo.Both.type = Formats.TypeInfo.Both
}
