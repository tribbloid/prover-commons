package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.diff.StringDiff

trait TermAndTypeOfs extends TypeOfs {

  class TermAndTypeOf[T](
      val rtValue: T,
      override val tt: universe.Type
  ) extends TypeOf(tt) {

    //  override def toString: String = {
    //    s"""
    //       |$value
    //       |
    //       |${tree.treeString}
    //       |""".trim.stripMargin
    //  }

    def ===!===(that: TermAndTypeOf[_] = null): Unit = {

      should_=:=(that)

      val Seq(s1, s2) = Seq(this, that).map { v =>
        Option(v).map(_.rtValue.toString)
      }

      val diff = StringDiff(s1, s2, Seq(this.getClass))

      (diff.Left.isDefined, diff.Right.isDefined) match {

        case (true, true) =>
          Predef.assert(
            this.rtValue == that.rtValue,
            diff.errorStr
          )

        case _ =>
          diff.show()
      }
    }
  }

}
