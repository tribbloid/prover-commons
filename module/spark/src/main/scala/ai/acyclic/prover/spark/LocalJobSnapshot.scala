package ai.acyclic.prover.spark

import org.apache.spark.SparkContext

case class LocalJobSnapshot(@transient ctx: SparkContext) {

  val groupID: String = ctx.getLocalProperty(Envs.SPARK_JOB_GROUP_ID)
  val description: String = ctx.getLocalProperty(Envs.SPARK_JOB_DESCRIPTION)
}
