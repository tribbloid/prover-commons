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
    *   - should only have 0 or 1 constructor arguments, the only arg is the tightest WeakTypeTag of itself
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

    implicit def summon[T <: Phantom: WeakTypeTag]: Case[T] = {
      summoningImpossible[T]
    }

    /**
      * create a concrete instance of [[T]] using JVM reflection and a [[WeakTypeTag]]
      *
      * This can be used to instantiate [[Phantom]] values that define methods and properties apart from dependent
      * types.
      *
      * always does the following:
      *
      *   - identify if [[T]] is a class or an object
      *     - if an object: return the object as-is
      *     - if a class with constructors:
      *       - use its 1-arg constructor (WeakTypeTag of itself) to create an instance
      *       - if not possible, use its 0-arg constructor to create an instance
      *       - if none of the above is possible, throw a runtime exception
      *   - cast it into [[T]]
      */
    def summonConcrete[T <: Phantom: WeakTypeTag]: Case[T] = {
      val mirror = scala.reflect.runtime.currentMirror
      val tpg = implicitly[WeakTypeTag[T]]
      val tpe = tpg.tpe
      val classSymbol = tpe.typeSymbol.asClass

      if (classSymbol.isAbstract && !classSymbol.isModuleClass) {
        val className = mirror.runtimeClass(classSymbol).getName
        throw new IllegalStateException(s"Cannot instantiate abstract Phantom class $className")
      }

      val instance = if (classSymbol.isModuleClass) {
        mirror.reflectModule(classSymbol.module.asModule).instance
      } else {
        val classMirror = mirror.reflectClass(classSymbol)
        val ctors = tpe.decl(scala.reflect.runtime.universe.termNames.CONSTRUCTOR).asTerm.alternatives
        
        val ctor1 = ctors.find(_.asMethod.paramLists.flatten.size == 1)
        val ctor0 = ctors.find(_.asMethod.paramLists.flatten.size == 0)

        ctor1.flatMap { c =>
          try {
            Some(classMirror.reflectConstructor(c.asMethod)(tpg))
          } catch {
            case _: IllegalArgumentException => None
          }
        }.orElse {
          ctor0.flatMap { c =>
            try {
              Some(classMirror.reflectConstructor(c.asMethod)())
            } catch {
              case _: IllegalArgumentException => None
            }
          }
        }.getOrElse {
          val className = mirror.runtimeClass(classSymbol).getName
          throw new IllegalArgumentException(s"Cannot find a 1-arg or nullary constructor for $className")
        }
      }

      new Case[T] {
        val out: T = instance.asInstanceOf[T]
      }
    }
  }

}
