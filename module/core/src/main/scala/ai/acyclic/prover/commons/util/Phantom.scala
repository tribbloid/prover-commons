package ai.acyclic.prover.commons.util

/**
  * static object that should never be instantiated
  *
  * used to circumvent lack of type projection in Scala 3, for example:
  *
  *   - Before: `type T = S#Dep`
  *   - After: `val s: S = Phantom(); type T = s.Dep`
  *
  * this can make some generic type declaration shorter: e.g. `T |- R` and `T |-\- R` can be grouped into *
  * `ForAll[T] { |-[R]; |-\-[R] }`, where `ForAll extends Case` * instances:
  *
  *   - should only contain dependent types, no method or property is allowed
  *   - method or property can exist in extension view
  */
trait Phantom extends Phantom.Case

object Phantom extends StaticGroup {

  implicit def onlyCase[T <: Case]: T = {
    throw new UnsupportedOperationException(
      "Phantom term should only be declare as lazy val for its dependent types, it should never be initialized"
    )
  }
}
