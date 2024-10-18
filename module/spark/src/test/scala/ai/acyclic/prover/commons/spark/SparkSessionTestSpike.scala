package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.testlib.BaseSpec
import org.scalatest.BeforeAndAfterAll

class SparkSessionTestSpike extends BaseSpec with BeforeAndAfterAll {

  override def beforeAll(): Unit = {}

  override def afterAll(): Unit = {}

  TestHelper.TestSC

  it("do nothing") {}

  it("simple map reduce") {

    TestHelper.TestSC.parallelize(1 to 100).map(v => v * v).reduce(_ + _)
  }
}
