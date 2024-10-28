package ai.acyclic.prover.commons.pending

import ai.acyclic.prover.commons.cap.Bound

trait PendingEffect extends Bound {}

object PendingEffect extends PendingGroup {

  //  trait Must[-S]

  // DO NOT CHANGE! will be delegated to kyo in Scala 3
  type <<[T, -S <: PendingEffect] >: T // | Must[S]
  // S is contravariant because Pending can be added implicitly

  private def __sanity(): Unit = {

    trait Ex

    trait IO1 extends PendingEffect {}

    trait IO2 extends PendingEffect {}

//      implicitly[(Ex << IO1) <:< (Ex << IO1 << IO2)] only works in Scala 3
//    implicitly[(Ex << IO1) <:< (Ex << IO2 << IO1)] should fail

    implicitly[(Ex << IO1) <:< (Ex << (IO1 with IO2))]
  }

  trait Universe extends PendingGroup {}
}
