package graph.commons.util.viz

import graph.commons.util.{indent, ScalaReflection}

import ScalaReflection.universe._

case class VizType(tt: Type) {

  import VizType._

  lazy val baseTypes: List[Type] = {

    val baseClzs = tt.baseClasses

    val result = baseClzs.map { clz =>
      tt.baseType(clz)
    }
    result
  }

  override def toString: String = {

    val rows = baseTypes.map { tt =>
      val paramTypes = tt.typeArgs

      val paramStrs = paramTypes.map { tt =>
        showFull(tt)
      }

      val paramViz = paramStrs.mkString("\n")

      val rootViz =
        s"""
           |${showFull(tt)}
           |${indent(paramViz)}
           |""".stripMargin.trim

      rootViz
    }

    s"""
       |${showFull(tt)}
       |${indent(rows.mkString("\n"))}
       """.trim.stripMargin
  }
}

object VizType {

  def showFull(tpe: Type): String = {
    s"$tpe \t:= ${showRaw(tpe.typeSymbol.typeSignature)}"
  }

  def apply[T](implicit ttag: TypeTag[T]): VizType = VizType(ttag.tpe)

  def infer[T: TypeTag](v: T): VizType = apply[T]
}
