package ai.acyclic.prover.commons.viz.text

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

  object Trim {

    lazy val perLine: TextBlock = {
      TextBlock(lines.map(_.trim))
    }

    lazy val left_right: TextBlock = {
      val noneEmptyLines = lines.filter { line =>
        line.trim.nonEmpty
      }

      val blankLeft = noneEmptyLines.map(_.indexWhere(v => !v.isWhitespace)).min
      val blankRight = noneEmptyLines.map(_.lastIndexWhere(v => !v.isWhitespace)).max

      val result = lines.map { line =>
        line.slice(blankLeft, blankRight + 1)
      }
      TextBlock(result)
    }

    lazy val top_bottom: TextBlock = {
      val blankTop = lines.indexWhere(_.trim.nonEmpty)
      val blankButtom = lines.lastIndexWhere(_.trim.nonEmpty)

      val result = lines.slice(blankTop, blankButtom + 1)
      TextBlock(result)
    }

    lazy val block: TextBlock = Trim.top_bottom.Trim.left_right
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

  def appendPerLine(
      that: TextBlock
  ): TextBlock = {

    val lines = this.lines
      .zipAll(that.lines, "", "")
      .map {
        case (a, b) =>
          a + b
      }

    TextBlock(lines)
  }

  override def toString: String = build
}

object TextBlock {

  def apply(tt: String) = new TextBlock(tt.split('\n').toIndexedSeq)

}
