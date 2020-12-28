package com.tribbloids.graph.commons.util

trait TreeFormat {

  lazy val DENOTE = " :"
  lazy val WRAP = " \u2223"

  lazy val FORK = "-+ "
  lazy val LEAF = "-- "

  lazy val INDENT = "  "

  def wText(tt: String) = new WText(tt.split('\n').toIndexedSeq)

  class WText(lines: Seq[String]) {

    lazy val build: String = lines.mkString("\n")

    def indent(ss: String = INDENT): WText = {

      new WText(lines.map(v => ss + v))
    }

    def prepend(
        ss: String,
        indent: String = INDENT
    ): WText = {

      val line1 = lines.headOption.map { head =>
        ss + head
      }.toSeq

      val remainder = lines.slice(1, Int.MaxValue).map { line =>
        indent + line
      }

      new WText(line1 ++ remainder)
    }
  }
}

object TreeFormat {

  object Indent2 extends TreeFormat
}
