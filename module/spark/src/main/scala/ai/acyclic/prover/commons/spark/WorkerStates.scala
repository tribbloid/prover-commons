package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.util.Caching.ConcurrentMap
import scala.collection.mutable

object WorkerStates {
  // stageID -> isExecuted
  val perWorkerMark: ConcurrentMap[Int, Boolean] = ConcurrentMap()

  // per-thread mark map of stage IDs that have executed "once-per-core" logic
  val perThreadMark: ThreadLocal[mutable.Map[Int, Boolean]] = new ThreadLocal[mutable.Map[Int, Boolean]] {
    override def initialValue(): mutable.Map[Int, Boolean] = mutable.Map.empty[Int, Boolean]
  }

}
