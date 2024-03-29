package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.testlib.BaseSpec
import org.apache.hadoop.security.UserGroupInformation
import org.scalatest.BeforeAndAfterAll

class SparkContextViewSuite extends BaseSpec with BeforeAndAfterAll {

  override def beforeAll(): Unit = {}

  TestHelper.TestSC

  describe("withJob") {

    it("can stack description") {

      SparkContextView.withJob("aaa") {

        SparkContextView.withJob("bbb") {

          TestHelper.TestSC.parallelize(1 to 100).map(v => v * v).collect()

          assert(SparkContextView.localJob.description == "aaa \u2023 bbb")
        }
      }
    }

    it("will not override existing groupID if not specified") {

      SparkContextView.withJob("aaa", "aaa") {

        SparkContextView.withJob("bbb") {

          TestHelper.TestSC.parallelize(1 to 100).map(v => v * v).collect()

          assert(SparkContextView.localJob.groupID == "aaa")
        }
      }
    }

    it("can override existing groupID") {

      SparkContextView.withJob("aaa", "aaa") {

        SparkContextView.withJob("bbb", "bbb") {

          UserGroupInformation.createRemoteUser("ccc")

          TestHelper.TestSC.parallelize(1 to 100).map(v => v * v).collect()

          assert(SparkContextView.localJob.groupID == "bbb")
        }
      }
    }
  }

  override def afterAll(): Unit = {}
}
