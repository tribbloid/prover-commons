package ai.acyclic.prover.commons.core.mix

object SPeer {
  def hello(): String = "hi from Scala"
  def greetFromJava(): String = JPeer.hello()
}
