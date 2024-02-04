package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons

import scala.language.implicitConversions

object Envs {

  implicit def asCoreEnvs(v: this.type): commons.Envs.type = commons.Envs

  val SPARK_JOB_DESCRIPTION: String = "spark.job.description"
  val SPARK_JOB_GROUP_ID: String = "spark.jobGroup.id"
  val SPARK_JOB_INTERRUPT_ON_CANCEL: String = "spark.job.interruptOnCancel"

  val RDD_SCOPE_KEY: String = "spark.rdd.scope"
  val RDD_SCOPE_NO_OVERRIDE_KEY: String = "spark.rdd.scope.noOverride"

  val METASTORE_PATH: String = this.USER_DIR :\ "metastore_db"
  val WAREHOUSE_PATH: String = this.USER_DIR :\ "warehouse"
}
