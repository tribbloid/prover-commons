package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.TypeTag

trait HasStatic {

  sealed trait Static extends Serializable

  /**
    * similar to Singleton, but all [[Case]] that has the same final type signature (including type arguments) are
    * assumed to be identical in runtime and congruent in compile-time. Serves as a counterpart of static generic class
    * in C#, or section with type variables in Lean4 prover
    *
    * e.g. for trait XX[T] extends Static, all (x: XX[Int]) should be identical (as in denotational equality, to be used
    * in Congruence lemma)
    *
    * [[Phantom]] is a special case of [[Case]] which cannot be summoned or constructed at runtime
    *
    *   - instances of [[StaticGroup]] should declare implicit cases which will be picked up by [[get_noCache]]
    *   - [[Case]] creation should always be interned by type argument
    */
  trait StaticGroup extends Serializable {

    trait Case extends Static

    @transient final lazy val cache: Caching.Strong._Cache[TypeTag[?], Case] = Caching.Strong.build()

    object get {

      def apply[T <: Case](
          implicit
          tag: TypeTag[T],
          ev: T
      ): T = {

        val result = cache.getOrElseUpdateOnce(tag)(ev)
        result.asInstanceOf[T]
      }
    }

    final def assume = get

    final def apply = get
  }

}
