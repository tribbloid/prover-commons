package ai.acyclic.prover.commons.viz.text

case class Padding(
    head: String,
    body: String,
    last: String,
    oneRow: String
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

  def keepHead(line: String): Padding = copy(body = line, last = line)

  def keepLast(line: String): Padding = copy(head = line, body = line)
}

object Padding {

  def ofHead(
      head: String,
      body: String
  ): Padding = Padding(head, body, body, head)

  def ofLast(
      body: String,
      last: String
  ): Padding = Padding(body, body, last, last)

  val argLeftBracket: Padding = ofHead(
    "┏ ",
    "┃ "
  )

  val leftParenthesis: Padding = Padding(
    "⎛ ",
    "┃ ",
    "⎝ ",
    "( "
  )

  val leftArrowUp: Padding = ofHead(
    "▲ ",
    "┃ "
  )

  val leftArrowDown: Padding = ofLast(
    "┃ ",
    "▼ "
  )
}
