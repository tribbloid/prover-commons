package graph.commons.util.viz

import graph.commons.util.{Type, TypeTag}

case class VizType(tpe: Type) {

  lazy val baseTypes: List[Type] = {

    val baseClzs = tpe.baseClasses

    val result = baseClzs.map { clz =>
      tpe.baseType(clz)
    }
    result
  }

  override def toString: String = {

    s"""
       |$tpe
       |${baseTypes.map(v => "\t- " + v).mkString("\n")}
       """.trim.stripMargin
  }
}

object VizType {

  def apply[T](implicit ttag: TypeTag[T]): VizType = VizType(ttag.tpe)

  def infer[T: TypeTag](v: T): VizType = apply[T]
}
