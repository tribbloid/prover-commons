package com.tribbloids.graph.commons.util

import com.tribbloids.graph.commons.util.TextBlock.Padding

trait TreeFormat {

  lazy val FORK: Padding = Padding("-+", " :")
  lazy val LEAF: Padding = Padding("--", "  ")

  lazy val SUB: Padding = Padding(" !", " :")
  lazy val SUB_LAST: Padding = SUB.copy(body = SUB.body.map(_ => ' '))

  lazy val DOT = " "

  case class Demo(
      nodeString: String,
      override val children: Seq[Demo] = Nil
  ) extends TreeLike {

    override lazy val format: TreeFormat = TreeFormat.this
  }
}

object TreeFormat {

  object Indent2 extends TreeFormat

  object Indent2Minimal extends TreeFormat {

    override lazy val FORK: Padding = Padding("", "")
    override lazy val LEAF: Padding = Padding("", "")
    override lazy val SUB: Padding = Padding(" ‣ ", " : ")

    override lazy val DOT = ""
  }
}
