package ai.acyclic.prover

package object commons {

  infix type <:<[A, +B] = Casting.<:<[A, B]

  infix type >:>[+B, A] = A <:< B

  type ->[+A, +B] = (A, B)

  type TypeTag[T] = scala.reflect.runtime.universe.TypeTag[T]
  type WeakTypeTag[T] = scala.reflect.runtime.universe.WeakTypeTag[T]
}
