package com.tribbloids.graph.commons.util

trait TreeFormat {

  import TreeFormat._

  lazy val FORK: Block = Block("-+", " :")
  lazy val LEAF: Block = Block("--", "  ")

  lazy val SUB: Block = Block(" !", " :")
  lazy val SUB_LAST: Block = SUB.copy(filler = SUB.filler.map(_ => ' '))

  lazy val DOT = " "

  def wText(tt: String) = new WText(tt.split('\n').toIndexedSeq)

  class WText(lines: Seq[String]) {

    lazy val build: String = lines.mkString("\n")

    def indent(ss: String): WText = {

      prepend(Block(ss, ss))
    }

    def prepend(
        block: Block
    ): WText = {

      val line1 = lines.headOption.map { head =>
        block.header + head
      }.toSeq

      val remainder = lines.slice(1, Int.MaxValue).map { line =>
        block.filler + line
      }

      new WText(line1 ++ remainder)
    }
  }

  case class Demo(
      nodeString: String,
      override val children: Seq[Demo] = Nil
  ) extends TreeLike {

    override lazy val format: TreeFormat = TreeFormat.this
  }
}

object TreeFormat {

  case class Block(
      header: String,
      filler: String
  ) {

    require(
      header.length == filler.length,
      s"prepend cannot use 2 strings with different lengths:" +
        s"\t`$header`" +
        s"\t`$filler`"
    )

  }

  object Indent2 extends TreeFormat

  object Indent2Minimal extends TreeFormat {

    override lazy val FORK: Block = Block("", "")
    override lazy val LEAF: Block = Block("", "")
    override lazy val SUB: Block = Block(" > ", " : ")

    override lazy val DOT = ""
  }
}
