package com.tribbloids.graph.commons.util

case class TextBlock(lines: Seq[String]) {

  import TextBlock._

  lazy val build: String = lines.mkString("\n")

  def indent(ss: String): TextBlock = {

    padLeft(Padding(ss, ss))
  }

  lazy val rectangular: TextBlock = {

    val longest = lines.map(_.length).max
    val withSpace = lines.map(v => v + (0 to (longest - v.length)).map(_ => ' ').mkString(""))
    TextBlock(withSpace)
  }

  def padLeft(
      pad: Padding
  ): TextBlock = {

    val line1 = lines.headOption.map { head =>
      pad.head + head
    }.toSeq

    val remainder = lines.slice(1, Int.MaxValue).map { line =>
      pad.body + line
    }

    new TextBlock(line1 ++ remainder)
  }

  def padRight(
      pad: Padding
  ): TextBlock = {
    val line1 = rectangular.lines.headOption.map { head =>
      head + pad.head
    }.toSeq

    val remainder = rectangular.lines.slice(1, Int.MaxValue).map { line =>
      line + pad.body
    }

    new TextBlock(line1 ++ remainder)
  }
}

object TextBlock {

  def apply(tt: String) = new TextBlock(tt.split('\n').toIndexedSeq)

  case class Padding(
      head: String,
      body: String
  ) {

    require(
      head.length == body.length,
      s"prepend cannot use 2 strings with different lengths:" +
        s"\t`$head`" +
        s"\t`$body`"
    )
  }

}