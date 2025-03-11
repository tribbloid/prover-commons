package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.util.Caching.ConcurrentMap


object RDDImplicitsStatic {

  // TODO: stageID is strictly incremental, this cost more memory than necessary
  //  a counter is good enough
  // (stageID -> threadID) -> isExecuted
  val perCoreMark: ConcurrentMap[(Int, Long), Boolean] = ConcurrentMap()
  // stageID -> isExecuted
  val perWorkerMark: ConcurrentMap[Int, Boolean] = ConcurrentMap()

}
