package ai.acyclic.prover

import ai.acyclic.prover.commons.multiverse.rewrite.HasCoercion

package object commons extends HasCoercion {

  object UpcastingRules extends HasCoercion {

    // TODO: <:< and >:> should become Coercion in this
    //  with non-cyclic verification
  }

  type >:>[+B, -A] = A <:< B

  type ->[+A, +B] = (A, B)

  type TypeTag[T] = scala.reflect.runtime.universe.TypeTag[T]
}
