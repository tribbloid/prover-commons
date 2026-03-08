package ai.acyclic.prover.commons.util

import scala.reflect.ClassTag

trait HasPhantom extends HasStatic {

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
      * This can be used to instantiate [[Phantom]] values that define methods and properties apart from dependent
      * types. For example, `ai.acyclic.prover.commons.jit.eval.HasArgs.Args.Schema` defines a `bottom` method.
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
      val runtimeClass = ev.runtimeClass

      val constructor =
        try {
          runtimeClass.getDeclaredConstructor()
        } catch {
          case err: NoSuchMethodException =>
            throw new IllegalArgumentException(
              s"Cannot instantiate phantom ${runtimeClass.getName}: nullary constructor not found",
              err
            )
        }

      constructor.setAccessible(true)

      val instance =
        try {
          constructor.newInstance()
        } catch {
          case err: ReflectiveOperationException =>
            throw new IllegalStateException(
              s"Cannot instantiate phantom ${runtimeClass.getName}",
              err
            )
        }

      ev.unapply(instance).getOrElse {
        throw new ClassCastException(
          s"Instantiated ${instance.getClass.getName} is not compatible with ${runtimeClass.getName}"
        )
      }
    }
  }

}
