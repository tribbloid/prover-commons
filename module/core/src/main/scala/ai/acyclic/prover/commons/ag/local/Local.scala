package ai.acyclic.prover.commons.ag.local

import ai.acyclic.prover.commons.ag.Batch
import ai.acyclic.prover.commons.ag.Engine

object Local extends Engine {

  case class _Batch[+V](
      collect: Seq[V]
  ) extends Batch[V] {}
}
