package ai.acyclic.prover.commons

trait TreeFormat {

  lazy val FORK: Padding = Padding("-+", " :")
  lazy val LEAF: Padding = Padding("--", "  ")

  lazy val SUB: Padding = Padding(" !", " :")
  lazy val SUB_LAST: Padding = SUB.copy(body = SUB.body.map(_ => ' '))

  lazy val DOT = " "
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
