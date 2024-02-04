package ai.acyclic.prover.commons.spark.locality

import org.apache.spark.Partitioner

class PartitionIdPassthrough(override val numPartitions: Int) extends Partitioner {

  override def getPartition(key: Any): Int = key.asInstanceOf[Int]
}
