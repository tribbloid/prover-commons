package ai.acyclic.prover.commons

object SPeer {
  def hello(): String = "hi from Scala"
  def greetFromJava(): String = JPeer.hello()
}
