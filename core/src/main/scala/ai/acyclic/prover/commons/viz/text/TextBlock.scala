package ai.acyclic.prover.commons.viz.text

case class TextBlock(lines: Seq[String]) {

  lazy val build: String = lines.mkString("\n")

  def indent(ss: String): TextBlock = {

    pad.left(Padding.ofHead(ss, ss))
  }

  lazy val rectangular: TextBlock = {

    val longest = lines.map(_.length).max
    val withSpace = lines.map(v => v + (0 until (longest - v.length)).map(_ => ' ').mkString(""))
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

    private def process(
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

    def left(pad: Padding) = process(pad, (vv, ll) => vv + ll)

    def right(pad: Padding) = process(pad, (vv, ll) => ll + vv)

//    def right(
//        pad: Padding
//    ): TextBlock = {
//      val line1 = rectangular.lines.headOption.map { head =>
//        head + pad.head
//      }.toSeq
//
//      val remainder = rectangular.lines.slice(1, Int.MaxValue).map { line =>
//        line + pad.body
//      }
//
//      new TextBlock(line1 ++ remainder)
//    }
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
