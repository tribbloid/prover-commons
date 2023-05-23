package ai.acyclic.prover.commons.typesetting

case class Padding(
    head: String,
    body: String,
    last: String,
    oneRow: String
) {

  {
    Seq(head, body, last).foreach { s =>
      require(s.split('\n').length == 1, "cannot use string with multiple lines")
    }

    require(
      Seq(head, body, last).map(_.length).distinct.length == 1,
      s"""
         |cannot use 2 strings with different lengths:
         |  `$head` - length=${head.length}
         |  `$body` - length=${body.length}
         |  `$last` - length=${last.length}
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

  val leftCurved: Padding = Padding(
    "⎛ ",
    "│ ",
    "⎝ ",
    "( "
  )

  val rightCurved: Padding = Padding(
    " ⎫",
    " │",
    " ⎭",
    " )"
  )

  val leftSquare: Padding = Padding(
    "┏ ",
    "┃ ",
    "┗ ",
    "⟦ "
  )

  val rightSquare: Padding = Padding(
    " ┓",
    " ┃",
    " ┛",
    " ⟧"
  )

  val leftArrowUp: Padding = ofHead(
    "▲ ",
    "┃ "
  )

  val leftArrowDown: Padding = ofLast(
    "┃ ",
    "▼ "
  )

  val vertical: Padding = ofHead(
    "|",
    "|"
  )
}
