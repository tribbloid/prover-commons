package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.testlib.BaseSpec
import org.apache.spark.SparkContext
import org.apache.spark.sql.SQLContext
import org.scalatest.BeforeAndAfterAll

trait SparkEnvSpec extends SparkEnvSpec.NoUISupport with SparkUISupport {}

object SparkEnvSpec {

  trait NoUISupport extends BaseSpec with BeforeAndAfterAll {

    final val sc: SparkContext = TestHelper.TestSC

    final val sql: SQLContext = TestHelper.TestSQL

    final def parallelism: Int = sc.defaultParallelism

    override def beforeAll(): Unit = {

      super.beforeAll()
      sc // initialize before all tests
    }

  }
}
