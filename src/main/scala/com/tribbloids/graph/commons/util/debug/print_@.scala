package com.tribbloids.graph.commons.util.debug

import com.tribbloids.graph.commons.util.debug.Debug.CallStackRef

case class print_@(
    exclusion: Seq[Class[_]]
) {

  def dryRun[T](
      v: T
  ): String = {

    val ref: CallStackRef = CallStackRef(exclude = Seq(this.getClass) ++ exclusion)

    val result: String =
      s"""
         |${v.toString}
         |\tat ${ref.showStr}
    """.stripMargin

    result
  }

  def apply[T](
      v: T
  ): Unit = {

    val result = dryRun(v)

    println(result)
  }
}

object print_@ extends print_@(Nil)
