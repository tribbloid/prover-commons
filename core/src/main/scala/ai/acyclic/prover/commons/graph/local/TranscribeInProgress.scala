package ai.acyclic.prover.commons.graph.local

import scala.collection.mutable

case class TranscribeInProgress[T](
    self: T
) {

  val arrows: mutable.Buffer[T] = mutable.Buffer.empty
}
