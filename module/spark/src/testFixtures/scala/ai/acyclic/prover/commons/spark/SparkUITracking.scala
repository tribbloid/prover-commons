package ai.acyclic.prover.commons.spark

import org.scalatest.{Status, Suite}

import java.util.concurrent.atomic.AtomicInteger

trait SparkUITracking extends Suite {

  abstract override protected def runTest(testName: String, args: org.scalatest.Args): Status = {

    lazy val fullText = s"[${this.suiteName}] $testName"

    SparkContextView(TestHelper.TestSC).withJob(fullText, SparkUITracking.serID.getAndIncrement().toString) {

      super.runTest(testName, args)
    }
  }

}

object SparkUITracking {

  val serID: AtomicInteger = new AtomicInteger()
}
