package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.util.SrcDefinition

class print_@()(
    implicit
    src: SrcDefinition
) {

  def wrapInfo[T](
      v: T
  ): String = {

    val result: String =
      s"""
         |${v.toString}
         |\tat ${src.longText}
    """.stripMargin

    result
  }

  def apply[T](
      v: T
  ): Unit = {

    val result = wrapInfo(v)

    println(result)
  }
}

object print_@ {

  def apply[T](
      v: T
  )(
      implicit
      src: SrcDefinition
  ): Unit = {

    new print_@().apply(v)
  }
}
