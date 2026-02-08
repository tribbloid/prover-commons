package ai.acyclic.prover.commons

object SPeer { // TODO: move to test

  def hello(): String = "hi from Scala"
  def greetFromJava(): String = JPeer.hello()
}
