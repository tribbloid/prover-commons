package ai.acyclic.prover.commons.cap

object __PendingSpike {

  type <[+T, -S] >: T

  trait E1

  trait E2

  type A = Int < E1
  type B = Int < E1 < E2
  type C = Int < E2 < E1

//  implicitly[A <:< B] // this shouldn't break, need to fix it before further impl?
  implicitly[A <:< C]
}
