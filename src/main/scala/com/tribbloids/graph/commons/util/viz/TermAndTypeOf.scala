package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.universe
import com.tribbloids.graph.commons.util.diff.StringDiff
import com.tribbloids.graph.commons.util.reflect.TypeFormat

class TermAndTypeOf[T](
    val value: T,
    override val tt: universe.Type,
    override val format: TypeFormat = TypeFormat()
) extends VizType(tt, format) {

//  override def toString: String = {
//    s"""
//       |$value
//       |
//       |${tree.treeString}
//       |""".trim.stripMargin
//  }

  def ===!===(that: TermAndTypeOf[_] = null): Unit = {

    shouldBe(that)

    val Seq(s1, s2) = Seq(this, that).map { v =>
      Option(v).map(_.value.toString)
    }

    val diff = StringDiff(s1, s2, Seq(this.getClass))

    (diff.Left.isDefined, diff.Right.isDefined) match {

      case (true, true) =>
        Predef.assert(
          this.tt =:= that.tt,
          diff.errorStr
        )

      case _ =>
        diff.show()
    }
  }
}
