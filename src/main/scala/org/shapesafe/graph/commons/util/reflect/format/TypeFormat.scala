package org.shapesafe.graph.commons.util.reflect.format

import org.shapesafe.graph.commons.util.reflect.Reflection

import scala.language.implicitConversions

trait TypeFormat {

  def resolve(refl: Reflection): refl.FormattedType => Output

  def joinText(v: Seq[String]): String = v.mkString(" ")

  def backtrack(ff: FormattedType): Nothing = {
    throw new Backtracking(
      s"Type ${ff.typeView} is not supported by format $this"
    )
  }

  implicit def fromText(v: String): Output = Output(v)

  implicit def fromText_Parts(v: (String, Seq[FormattedType])): Output = Output(v._1, v._2)

  def ~(factory: TypeFormat => TypeFormat): TypeFormat = factory(this)

  // TODO: the following should be moved into a view that also contains TypeVizFormat
  //  should make compile-time macro much easier to define
  lazy val DeAlias = Formats1.DeAlias(this)

  lazy val HidePackage = Formats1.Hide.HidePackage(this)
  lazy val HideStatic = Formats1.Hide.HideStatic(this)

  lazy val Both = Formats1.Concat(
    this.DeAlias,
    this
  )

}

object TypeFormat {

  val Default: Formats0.TypeInfo.Both.type = Formats0.TypeInfo.Both
}
