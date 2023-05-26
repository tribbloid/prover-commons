package ai.acyclic.prover.commons.meta2.format

import ai.acyclic.prover.commons.meta2.Reflection

import scala.language.implicitConversions

trait TypeFormat {

  def resolve(refl: Reflection): refl.TypeView => IROutput

  def joinText(v: Seq[String]): String = v.mkString(" ")

  def backtrack(tt: TypeView): Nothing = {
    throw new Backtracking(
      s"Type $tt is not supported by format $this"
    )
  }

  implicit def fromText(v: String): IROutput = IROutput(v)

//  implicit def fromText_Parts(v: (String, Seq[FormattedType])): IROutput = IROutput(v._1, v._2)

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

  trait Constructor extends (TypeFormat => TypeFormat) {

    def Format: TypeFormat => TypeFormat

    final def apply(v: TypeFormat) = Format(v)
  }
}
