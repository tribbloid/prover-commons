package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

import org.scalatest.BeforeAndAfterEach

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

class CachingSpec extends BaseSpec with BeforeAndAfterEach {

  import CachingSpec._
  implicit def global: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  override def beforeEach(): Unit = {
    count = 0
  }

  override def afterEach(): Unit = {
    count = 0
  }

  def createData(): Unit = {
    CacheTestData()
  }

  describe("spike") {
    it("exit from a subroutine allows all referenced objected to be GC'ed") {

      createData()

      //      Thread.sleep(10000)
      System.gc()
      Thread.sleep(2000)

      assert(count == 1)
    }

    it("termination of thread allows all referenced objected to be GC'ed") {

      val f: Future[Unit] = Future {

        CacheTestData()
      }
      Await.result(f, Duration.Inf)

      //      Thread.sleep(10000)
      System.gc()
      Thread.sleep(2000)

      assert(count == 1)
    }
  }

  describe("Weak ConcurrentCache") {

    describe("should remove value on garbage collection") {

      it("if the value is de-referenced") {
        val cache = Caching.Weak.View[String, CacheTestData]()

        var myVal = CacheTestData("myString")

        cache.put("a", myVal)
        myVal = null

        System.gc()
        Thread.sleep(10) // delay to allow gc

        assert(count == 1)
      }

      it("if the value is not in scope") {

        val cache = Caching.Weak.View[String, CacheTestData]()

        val f: Future[Unit] = Future {

          val v1 = CacheTestData()

          cache += "a" -> v1
        }
        Await.result(f, Duration.Inf)

        System.gc()
        Thread.sleep(2000)

        assert(count == 1)
      }
    }
  }
}

object CachingSpec {

  @volatile var count: Int = 0

  case class CacheTestData(s: String = "") {
    // called when object is garbage collected, which allows verification of gc behaviour
    override def finalize(): Unit = {
      super.finalize()
      count += 1
    }
  }
}
