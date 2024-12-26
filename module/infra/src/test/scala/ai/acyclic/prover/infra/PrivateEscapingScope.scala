package ai.acyclic.prover.infra

object PrivateEscapingScope {

  trait T1 {

    object R1
  }

  trait K1 {
    private lazy val t1: T1 = new T1 {}

    def r1: Any = t1.R1
  }

  trait K2 {
    private def t1: T1 = new T1 {}

    def r1 = t1.R1
  }
}
