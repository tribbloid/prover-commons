package com.tribbloids.graph.commons.util.debug

import com.tribbloids.graph.commons.util.debug.Debug.CallStackRef

case class print_@(
    exclusion: Seq[Class[_]]
) {

  def apply[T](
      v: T
  ): Unit = {

    val ref: CallStackRef = CallStackRef(exclude = Seq(this.getClass) ++ exclusion)

    val result: String =
      s"""
         |${v.toString}
         |\tat ${ref.showStr}
    """.stripMargin

    println(result)
  }
}

object print_@ extends print_@(Nil)
