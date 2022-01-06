package ai.acyclic.graph.commons

case class TextBlock(lines: Seq[String]) {

  lazy val build: String = lines.mkString("\n")

  def indent(ss: String): TextBlock = {

    padLeft(Padding(ss, ss))
  }

  lazy val rectangular: TextBlock = {

    val longest = lines.map(_.length).max
    val withSpace = lines.map(v => v + (0 until (longest - v.length)).map(_ => ' ').mkString(""))
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

  def zipRight(
      that: TextBlock
  ): TextBlock = {

    val maxLines = Seq(this, that).map(_.lines.size).max
    val expanded = Seq(this, that).map { block =>
      TextBlock(block.lines.padTo(maxLines, ""))
    }

    val zipped = expanded.head.rectangular.lines.zip(expanded(1).lines).map { v =>
      v._1 + v._2
    }

    TextBlock(zipped)
  }
}

object TextBlock {

  def apply(tt: String) = new TextBlock(tt.split('\n').toIndexedSeq)

}
