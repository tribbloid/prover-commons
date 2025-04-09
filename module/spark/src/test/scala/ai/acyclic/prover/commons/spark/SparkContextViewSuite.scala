package ai.acyclic.prover.commons.spark

import org.apache.hadoop.security.UserGroupInformation
import org.scalatest.BeforeAndAfterAll

class SparkContextViewSuite extends SparkEnvSpec with BeforeAndAfterAll {

  override def beforeAll(): Unit = {}

  override def afterAll(): Unit = {}

  TestHelper.TestSC

  val caseName = "job description"

  it(caseName) {

    SparkContextView.withJob("aaa") {

      SparkContextView.withJob("bbb") {

        TestHelper.TestSC.parallelize(1 to 100).map(v => v * v).collect()

        assert(SparkContextView.localJob.description == "[SparkContextViewSuite] job description \u2023 aaa \u2023 bbb")
      }
    }
  }

  describe("withJob") {

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
}
