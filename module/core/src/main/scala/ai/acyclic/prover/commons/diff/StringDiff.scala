package ai.acyclic.prover.commons.diff

import ai.acyclic.prover.commons.debug.print_@
import ai.acyclic.prover.commons.typesetting.TextBlock
import ai.acyclic.prover.commons.util.SrcDefinition

case class StringDiff(
    left: Option[String],
    right: Option[String],
    classes: Seq[Class[?]] = Nil,
    sort: Boolean = false,
    ignoreCase: Boolean = false,
    trim: TextBlock => TextBlock = StringDiff.defaultTrim
) {

  import StringDiff.*

  private val _printFn = new print_@()(
    SrcDefinition.Runtime(
      classes :+
        this.getClass
    )
  )

  case class Rows(
      raw: Option[String],
      header: String
  ) {

    def outer: StringDiff = StringDiff.this

    def isDefined: Boolean = raw.isDefined

    val trimmed: Option[TextBlock] = raw.map(v => trim(TextBlock(v)))

    val rows: Vector[String] = trimmed.toVector.flatMap { raw =>
      raw.lines
    }

    val effective: Vector[String] = {

      var a = rows.map(v => ("|" + v).trim.stripPrefix("|"))

      if (sort) a = a.sorted
      if (ignoreCase) a = a.map(_.toLowerCase)
      a
    }

    object info {

      lazy val rows: Vector[String] = {
        Vector(
          s"\n=============================== $header ================================",
          ""
        ) ++ effective
      }

      def textWithMargin(margin: String): String = {
        rows
          .map(v => margin + v)
          .mkString("\n")
      }

      lazy val text: String = textWithMargin("")

      override lazy val toString: String = textWithMargin("|") // to be used for interpolation with stripMargin
    }
  }

  object Right extends Rows(right, "[EXPECTED / RIGHT]")
  object Left extends Rows(left, "[ACTUAL   /  LEFT]")

  object ErrorDiffClue {

    lazy val error1: String = {

      s"""
           |expected: <
           ${Left.info}
           |> but was: <
           ${Right.info}
           |>
      """.stripMargin
    }

    lazy val error1b: String = {

      s"""
           |expected: "
           ${Left.info}
           |" but was: "
           ${Right.info}
           |"
      """.stripMargin
    }

    lazy val error2: String = {

      s"""
           |<
           ${Left.info}
           |> did not equal <
           ${Right.info}
           |>
      """.stripMargin
    }

    lazy val error2b: String = {

      s"""
           |"
           ${Left.info}
           |" did not equal "
           ${Right.info}
           |"
      """.stripMargin
    }

    override lazy val toString: String = error2.trim

    def isEqual: Boolean = Left.effective == Right.effective
  }

  lazy val plainDiffClue: String = {
    s"""
         ${Left.info}
         |
         ${Right.info}
         |""".stripMargin.trim
  }

  lazy val info: String = {

    (left.isDefined, right.isDefined) match {

      case (true, true) =>
        plainDiffClue
      case (false, false) =>
        throw new UnsupportedOperationException("both left and right operands are missing")
      case (true, false) =>
        Left.info.text
      case (false, true) =>
        Right.info.text
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

  lazy val defaultTrim: TextBlock => TextBlock = { (v: TextBlock) =>
    v.trim.top_bottom.trim.carriageReturn
  }
}
