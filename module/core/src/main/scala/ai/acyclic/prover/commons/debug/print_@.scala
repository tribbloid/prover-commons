package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.util.SrcDefinition

case class print_@(
    src: SrcDefinition
) {

  def wrapInfo[T](
      v: T
  ): String = {

    val result: String =
      s"""
         |${v.toString}
         |\tat ${src.toString}
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

object print_@ extends print_@(SrcDefinition.Runtime(Seq(classOf[print_@]))())
