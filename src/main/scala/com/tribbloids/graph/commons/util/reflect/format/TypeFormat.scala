package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection

import scala.language.implicitConversions

trait TypeFormat {

  def resolve(refl: Reflection): refl.Formatting => Output

  def joinText(v: Seq[String]): String = v.mkString(" ")

  def backtrack(ff: Formatting): Nothing = {
    throw new Backtracking(
      s"Type ${ff.typeView} is not supported by format $this"
    )
  }

  implicit def fromText(v: String): Output = Output(v)

  implicit def fromTuple(v: (String, Seq[Formatting])): Output = Output(v._1, v._2)

  implicit def fromEquivalent(v: (Formatting, Formatting)): Output = {
    val sameFormat = v._2.format == this
    val sameType = v._1.typeView == v._2.typeView

    require(
      !(sameFormat && sameType),
      "cannot convert Formatting into Output: may trigger dead loop"
    )
    Output(v._2.text, parts = v._2.parts, equivalent = Option(v._2))
  }

  def ~(factory: TypeFormat => TypeFormat): TypeFormat = factory(this)

  // TODO: the following should be moved into a view that also contains TypeVizFormat
  //  should make compile-time macro much easier to define
  lazy val DeAlias = FormatProtos.DeAlias(this)

  lazy val HidePackage = FormatProtos.Hide.Package(this)
  lazy val HideStatic = FormatProtos.Hide.Static(this)

  lazy val Both = FormatProtos.Concat(
    this.DeAlias,
    this
  )

}

object TypeFormat {

  val Default: Formats.TypeInfo.Both.type = Formats.TypeInfo.Both
}
