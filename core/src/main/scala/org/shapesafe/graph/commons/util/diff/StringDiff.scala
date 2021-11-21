package org.shapesafe.graph.commons.util.diff

import org.shapesafe.graph.commons.util.debug.print_@

case class StringDiff(
    left: Option[String],
    right: Option[String],
    classes: Seq[Class[_]] = Nil,
    sort: Boolean = false,
    ignoreCase: Boolean = false
) {

  import StringDiff._

  val _print = new print_@(
    classes :+
      this.getClass
  )

  case class Rows(
      raw: Option[String],
      meta: String
  ) {

    def outer: StringDiff = StringDiff.this

    def isDefined: Boolean = raw.isDefined

    val rows: List[String] = raw.toList.flatMap { raw =>
      raw
        .split("\n")
        .toList
    }

    val effective: List[String] = {

      var a = rows
        .filterNot(_.replace(" ", "").isEmpty)
        .map(v => ("|" + v).trim.stripPrefix("|"))

      if (sort) a = a.sorted
      if (ignoreCase) a = a.map(_.toLowerCase)
      a
    }

    lazy val str: String = effective.mkString("\n")
    lazy val info: String = (
      s"\n=============================== $meta ================================\n\n" +
        str
    ).trim

  }

  object Left extends Rows(left, "[ACTUAL   /  LEFT]")
  object Right extends Rows(right, "[EXPECTED / RIGHT]")

  lazy val errorStr: String = {

    val result = "\n" + s"""
                           |"
                           |${Left.info}
                           |" did not equal "
                           |${Right.info}
                           |"
      """.stripMargin.trim

    result
  }

  lazy val info: String = {

    (left.isDefined, right.isDefined) match {

      case (true, true) =>
        """
          |${Left.info}
          |
          |${Right.info}
          |""".stripMargin.trim

      case (false, false) =>
        throw new UnsupportedOperationException("both left and right operands are missing")
      case (true, false) =>
        Left.info
      case (false, true) =>
        Right.info
    }
  }

  def show(): Unit = {
    _print(info)
  }

  def assert(
      mode: StringDiff.ComparisonMode = StringDiff.Equal,
      fuzzyRight: Boolean = false
  ): Unit = {

    def assertEqual(left: Seq[String], right: Seq[String]): Unit = {

      if (fuzzyRight) {

        left.zipAll(right, null, null).foreach { tuple =>
          val fixes = tuple._2.split("[.]{6,}", 2)
          Predef.assert(
            tuple._1.startsWith(fixes.head),
            errorStr
          )
          Predef.assert(
            tuple._1.endsWith(fixes.last),
            errorStr
          )
        }
      } else {

        Predef.assert(
          left == right,
          errorStr
        )
      }
    }

    (left.isDefined, right.isDefined) match {

      case (true, true) =>
        mode match {
          case SuperSet =>
            assertEqual(Left.effective.intersect(Right.effective), Right.effective)

          case SubSet =>
            assertEqual(Left.effective, Left.effective.intersect(Right.effective))

          case Equal =>
            assertEqual(Left.effective, Right.effective)

        }

      case _ =>
        show()
    }
  }
}

object StringDiff {

  sealed trait ComparisonMode
  object Equal extends ComparisonMode
  object SuperSet extends ComparisonMode
  object SubSet extends ComparisonMode
}
