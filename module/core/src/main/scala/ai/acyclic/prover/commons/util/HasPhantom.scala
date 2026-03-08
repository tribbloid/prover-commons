package ai.acyclic.prover.commons.util

import scala.reflect.ClassTag

trait HasPhantom {

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
  type Phantom = Phantom.Case

  object Phantom extends StaticGroup {

    implicit def summon[T <: Case]: T = {
      throw new UnsupportedOperationException(
        "Phantom term should only be declare as lazy val for its dependent types, it should never be initialized"
      )
    }

    /**
      * create a concrete instance of [[T]] using JVM reflection and the given [[ClassTag]]
      *
      * This can be used instantiate [[Phantom]] that defined methods & properties apart from dependent types. E.g.
      * [[jit.eval.Args.Schema]], which defined "bottom" method. // TODO: fix all links
      *
      * always does the following:
      *
      *   - first identify the nullary constructor of the class, throw a runtime exception if not found
      *   - instantiate it
      *   - cast it into [[T]]
      */
    def summonConcrete[T <: Phantom](
        ev: ClassTag[T]
    ): T = {
      ???
    }
  }

}
