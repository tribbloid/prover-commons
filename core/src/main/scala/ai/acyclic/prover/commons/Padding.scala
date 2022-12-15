package ai.acyclic.prover.commons

case class Padding(
    head: String,
    body: String
) {

  {
    Seq(head, body).foreach { s =>
      require(s.split('\n').length == 1, "cannot use string with multiple lines")
    }

    require(
      head.length == body.length,
      s"""
         |cannot use 2 strings with different lengths:
         |  `$head` - length=${head.length}
         |  `$body` - length=${body.length}
         |""".stripMargin.trim
    )
  }
}

object Padding {

  val argLeftBracket: Padding = Padding(
    "┏ ",
    "┃ "
  )

  val leftArrowUp: Padding = Padding(
    "▲ ",
    "┃ "
  )
}
