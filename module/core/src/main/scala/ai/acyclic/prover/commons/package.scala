package ai.acyclic.prover

import ai.acyclic.prover.commons.multiverse.rewrite.HasCoercion

package object commons extends HasCoercion {

  type >:>[+B, -A] = A <:< B

  type ->[+A, +B] = (A, B)

  type TypeTag[T] = scala.reflect.runtime.universe.TypeTag[T]
}
