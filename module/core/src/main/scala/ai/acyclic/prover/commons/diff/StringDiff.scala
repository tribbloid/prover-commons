package ai.acyclic.prover.commons.diff

import ai.acyclic.prover.commons.debug.print_@
import ai.acyclic.prover.commons.typesetting.TextBlock

case class StringDiff(
    left: Option[String],
    right: Option[String],
    classes: Seq[Class[_]] = Nil,
    sort: Boolean = false,
    ignoreCase: Boolean = false,
    trim: TextBlock => TextBlock = StringDiff.defaultTrim
) {

  import StringDiff._

  private val _printFn = new print_@(
    classes :+
      this.getClass
  )

  case class Rows(
      raw: Option[String],
      header: String
  ) {

    def outer: StringDiff = StringDiff.this

    def isDefined: Boolean = raw.isDefined

    val trimmed: Option[TextBlock] = raw.map(v => trim(TextBlock(v)))

    val rows: List[String] = trimmed.toList.flatMap { raw =>
      raw.lines
    }

    val effective: List[String] = {

      var a = rows.map(v => ("|" + v).trim.stripPrefix("|"))

      if (sort) a = a.sorted
      if (ignoreCase) a = a.map(_.toLowerCase)
      a
    }

    lazy val str: String = effective.mkString("\n")
    lazy val info: String = (
      s"\n=============================== $header ================================\n\n" +
        str
    ).trim

  }

  object Right extends Rows(right, "[EXPECTED / RIGHT]")
  object Left extends Rows(left, "[ACTUAL   /  LEFT]")

  object ErrorDiffClue {

    lazy val error1: String = {

      s"""
           |expected: <
           |${Left.info}
           |> but was: <
           |${Right.info}
           |>
      """.stripMargin.trim
    }

    lazy val error1b: String = {

      s"""
         |expected: "
         |${Left.info}
         |" but was: "
         |${Right.info}
         |"
      """.stripMargin.trim
    }

    lazy val error2: String = {

      s"""
         |<
         |${Left.info}
         |> did not equal <
         |${Right.info}
         |>
      """.stripMargin.trim
    }

    lazy val error2b: String = {

      s"""
           |"
           |${Left.info}
           |" did not equal "
           |${Right.info}
           |"
      """.stripMargin.trim
    }

    override lazy val toString: String = error2

    def isEqual = Left.effective == Right.effective
  }

  lazy val plainDiffClue: String = {
    s"""
       |${Left.info}
       |
       |${Right.info}
       |""".stripMargin.trim
  }

  lazy val info: String = {

    (left.isDefined, right.isDefined) match {

      case (true, true) =>
        plainDiffClue
      case (false, false) =>
        throw new UnsupportedOperationException("both left and right operands are missing")
      case (true, false) =>
        Left.info
      case (false, true) =>
        Right.info
    }
  }

  def show(): Unit = {
    _printFn(info)
  }

  def assert[T](
      mode: StringDiff.ComparisonMode = StringDiff.Equal,
      fuzzyRight: Boolean = false,
      usingFn: (Boolean, Any) => Any = Predef.assert(_, _)
  ): Unit = {

    def assertEqual(left: Seq[String], right: Seq[String]): Unit = {

      if (fuzzyRight) {
        // TODO: need improvement using some simple parser

        left.zipAll(right, null, null).foreach { tuple =>
          val fixes = tuple._2.split("[.]{6,}", 2)
          usingFn(
            tuple._1.startsWith(fixes.head),
            ErrorDiffClue
          )
          usingFn(
            tuple._1.endsWith(fixes.last),
            ErrorDiffClue
          )
        }
      } else {

        usingFn(
          left == right,
          ErrorDiffClue
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

  lazy val defaultTrim: TextBlock => TextBlock = { v: TextBlock =>
    v.trim.top_bottom
  }
}
