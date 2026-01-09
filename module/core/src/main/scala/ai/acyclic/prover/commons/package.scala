package ai.acyclic.prover

package object commons {

  type >:>[+B, -A] = A <:< B

  type ->[+A, +B] = (A, B)
}
