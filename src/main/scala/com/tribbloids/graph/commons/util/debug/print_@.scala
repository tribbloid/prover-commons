package com.tribbloids.graph.commons.util.debug

import com.tribbloids.graph.commons.util.debug.Debug.CallStackRef

case object print_@ {

  def apply[T](v: T): Unit = {

    val ref: CallStackRef = CallStackRef(exclude = Seq(this.getClass))

    val result: String =
      s"""
         |${v.toString}
         |\tat ${ref.showStr}
    """.stripMargin

    println(result)
  }
}
