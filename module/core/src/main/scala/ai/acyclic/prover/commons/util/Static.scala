package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.TypeTag

/**
  * similar to Singleton, but all instances that hs the same type signature should be the same object, not necessarily
  * globally unique
  *
  * e.g. for trait XX[T] extends Static, all (x: XX[Int]) should be the same object (as in denotational equality, to be
  * used in Congruence lemma)
  *
  * object creation should be interned
  */

sealed trait Static

//object Static {
//
//  trait Group {
//
//    trait Impl extends Static {}
//
//    def get[T <: Impl]: T
//
//    final def apply[T <: Impl](): T = get[T]
//  }
//}

trait StaticGroup {

  trait Case

  val cache: Caching.Strong._Cache[TypeTag[?], Case & Static] = Caching.Strong.build()

  private def get_noCache[T <: Case](
      implicit
      ev: Case
  ): Case = ev

  object get {

    def get[T <: Case](
        implicit
        tag: TypeTag[T],
        ev: Case
    ): Case & Static = {

      cache.getOrElseUpdateOnce(tag)(ev.asInstanceOf[Case & Static])
    }
  }

  final def assume = get

  final def apply = get
}
