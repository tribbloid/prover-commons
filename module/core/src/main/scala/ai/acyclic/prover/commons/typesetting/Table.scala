package ai.acyclic.prover.commons.typesetting

object Table {

  case class Cell(v: Any) {

    override lazy val toString: String = "" + v

    lazy val displayLength: Int = {

      val escapeRemoved = v.toString.replaceAll("\u001b\\[[;\\d]*m", "")
      escapeRemoved.length
    }
  }
}

case class Table(
    table: Seq[Seq[Any]]
) {
  // stolen from Apache Spark SQL

  import Table.*

  def formattedText: String = {

    val result: String = if (table.isEmpty) {
      ""
    } else {
      val _table = table.map { row =>
        row.map { v =>
          Cell(v)
        }
      }

      val colWidths = _table.transpose.map(_.map { cell =>
        cell.displayLength
      }.max + 2)

      val rows = _table.map(
        _.zip(colWidths)
          .map {
            case (cell, size) =>
              s" ${cell.toString} " + Array.fill(size - cell.displayLength - 2)(' ').mkString

          }
          .mkString("|", "|", "|")
      )

      //      val separator = colWidths.map("-" * _).mkString("+", "+", "+")
      // Put the table together and return
      //      (separator +: rows.head +: separator +: rows.tail :+ separator).mkString("\n")

      (rows.head +: rows.tail).mkString("\n")
    }
    result

  }
}
