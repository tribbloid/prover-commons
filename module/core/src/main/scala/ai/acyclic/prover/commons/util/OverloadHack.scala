package ai.acyclic.prover.commons.util

/**
  * see https://web.stanford.edu/class/cs442/lectures_unrestricted/cs442-lms.pdf for explanation
  *
  * Force the compiler to distinguish the method types by attaching a different implicit parameter to each signature
  *
  * At least only the DSL authors (you guys) see it, and not the users ...
  */
trait OverloadHack {
  // TODO: should be generated

  class Overloaded1
  class Overloaded2
  class Overloaded3
  class Overloaded4

  implicit val overloaded1: Overloaded1 = new Overloaded1
  implicit val overloaded2: Overloaded2 = new Overloaded2
  implicit val overloaded3: Overloaded3 = new Overloaded3
  implicit val overloaded4: Overloaded4 = new Overloaded4
}
