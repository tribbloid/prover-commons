package ai.acyclic.prover.commons.viz.text

case class TextBlock(lines: Seq[String]) {

  lazy val build: String = lines.mkString("\n")

  def indent(ss: String): TextBlock = {

    pad.left(Padding.ofHead(ss, ss))
  }

  object numberOf {

    lazy val rows = lines.size

    lazy val columns = lines.map(_.length).max
  }

  lazy val rectangular: TextBlock = {

    val withSpace = lines.map(v => v + (0 until (numberOf.columns - v.length)).map(_ => ' ').mkString(""))
    TextBlock(withSpace)
  }

  def foldEmptyLines: TextBlock = TextBlock(
    lines.filter(ll => ll.nonEmpty)
  )

  object trim {

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

    lazy val block: TextBlock = trim.top_bottom.trim.left_right
  }

  object pad {

    private def processRectangular(
        pad: Padding,
        forEachLine: (String, String) => String
    ): TextBlock = {

      if (lines.length <= 1) TextBlock(lines.map { ll =>
        forEachLine(pad.oneRow, ll)
      })
      else {

        val firstLine = lines.headOption.map { ll =>
          forEachLine(pad.head, ll)
        }.toSeq

        val lastLine = lines.lastOption.map { ll =>
          forEachLine(pad.last, ll)
        }

        val inBetween = lines.slice(1, lines.size - 1).map { ll =>
          forEachLine(pad.body, ll)
        }

        new TextBlock(firstLine ++ inBetween ++ lastLine)
      }
    }

    private def process(
        pad: Padding,
        forEachLine: (String, String) => String
    ): TextBlock = {
      rectangular.pad.processRectangular(pad, forEachLine)
    }

    def left(pad: Padding) = process(pad, (vv, ll) => vv + ll)

    def right(pad: Padding) = process(pad, (vv, ll) => ll + vv)

    def top(char: Char): TextBlock = {
      TextBlock(
        Seq(char.toString * numberOf.columns) ++ lines
      )
    }

    def bottom(char: Char): TextBlock = {
      TextBlock(
        lines :+ (char.toString * numberOf.columns)
      )
    }
  }

  object encloseIn {

    def parenthesis: TextBlock = pad.left(Padding.leftCurved).pad.right(Padding.rightCurved)

    def squareBracket: TextBlock = pad.left(Padding.leftSquare).pad.right(Padding.rightSquare)
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

  def zipBottom(
      that: TextBlock
  ): TextBlock = {

    copy(lines ++ that.lines)
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

  def apply(tt: String) = new TextBlock(tt.split('\n').toVector)

}
