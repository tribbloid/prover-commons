package com.tribbloids.graph.commons.util.debug

import com.tribbloids.graph.commons.util.debug.Debug.CallStackRef

case class print_@[T](v: T) {

  val ref: CallStackRef = CallStackRef(exclude = Seq(this.getClass))

  val result: String =
    s"""
       |${v.toString}
       |\tat ${ref.showStr}
    """.stripMargin

  println(result)
}
