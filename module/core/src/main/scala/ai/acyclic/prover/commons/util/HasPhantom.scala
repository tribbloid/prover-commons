package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.WeakTypeTag

trait HasPhantom extends HasStatic {

  /**
    * static, compile-time-only object with extremely lightweight constructor, it may be constructed at runtime for its
    * terms/methods, but this is not recommended (until Scala 3, in which construction can be avoided 100% by opaque
    * type or polymorphic extension view)
    *
    * used to circumvent lack of type projection in Scala 3, for example:
    *
    *   - Before: `type T = S#Dep`
    *   - After: `val s: S = Phantom(); type T = s.Dep`
    *
    * this can be used to aggregate shared type arguments of many generics to make their declarations shorter: e.g.
    * `T |- R` and `T |-\- R` can be grouped into * `ForAll[T] { |-[R]; |-\-[R] }`, where `ForAll extends Case`.
    * Assuming lambdaP2-lambdaC conjecture, it is functionally identical to LEAN4 section or C# static generic class.
    *
    * instances:
    *   - should only have 0 explicit constructor arguments
    *   - can contain dependent type members safely
    *   - can contain properties or methods directly (not recommended) or through extension view
    */
  abstract class Phantom private (maybeTypeTag: Option[WeakTypeTag[?]]) extends Static {

    def this() = this(None)

    def this(ttg: WeakTypeTag[?]) = this(Some(ttg))
  }

  trait Phantom_Imp0 extends StaticGroup[Phantom] {

    implicit def summoningImpossible[T <: Phantom]: Case[T] = {
      throw new UnsupportedOperationException(
        "Phantom term should only be declare as lazy val for its dependent types, it should never be initialized"
      )
    }
  }

  object Phantom extends Phantom_Imp0 {

    // TODO: don't know how to do it at the moment
//    implicit def summon[T <: Phantom: WeakTypeTag]: Case[T] = {
//      summoningImpossible[T]
//    }
  }
}
