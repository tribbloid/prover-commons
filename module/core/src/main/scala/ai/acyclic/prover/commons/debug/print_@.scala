package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

case class print_@(
    exclusion: Seq[Class[_]]
) {

  def wrapInfo[T](
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

    val result = wrapInfo(v)

    println(result)
  }
}

object print_@ extends print_@(Nil)
