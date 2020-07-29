package graph.commons.util.debug

import graph.commons.util.debug.Debug.CallStackRef

case class print_@[T](v: T) {

  val ref: CallStackRef = CallStackRef(exclude = Seq(this.getClass))

  println(
    s"""
         |${v.toString}
         |\tat ${ref.showStr}
         """.stripMargin
  )
}
