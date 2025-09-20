package ai.acyclic.prover.commons.debug

object delay_@ {

  def apply(
      timeMillis: Long
  )(
      implicit
      src: SrcDefinition
  ): Unit = {

    print_@(s"delaying for $timeMillis ms")(src)

    Thread.sleep(timeMillis)
  }
}
