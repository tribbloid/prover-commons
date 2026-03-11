package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec
import org.scalatest.BeforeAndAfterEach

import java.lang.ref.Cleaner
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

class CachingSpec extends BaseSpec with BeforeAndAfterEach {

  import CachingSpec.*
  implicit def global: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  override def beforeEach(): Unit = {
    startTracking()
  }

  override def afterEach(): Unit = {
    startTracking()
  }

  def createData(): Unit = {
    CacheTestData()
  }

  describe("spike") {
    it("exit from a subroutine allows all referenced objected to be GC'ed") {

      createData()

      waitForCollection(1)
    }

    it("termination of thread allows all referenced objected to be GC'ed") {

      val f: Future[Unit] = Future {

        CacheTestData()
      }
      Await.result(f, Duration.Inf)

      waitForCollection(1)
    }
  }

  describe("Weak ConcurrentCache") {

    describe("should remove value on garbage collection") {

      it("if the value is de-referenced") {
        val cache = Caching.Weak._Cache[String, CacheTestData]()

        var myVal = CacheTestData("myString")

        cache.put("a", myVal)
        myVal = null

        waitForCollection(1)
      }

      it("if the value is not in scope") {

        val cache = Caching.Weak._Cache[String, CacheTestData]()

        val f: Future[Unit] = Future {

          val v1 = CacheTestData()

          cache += "a" -> v1
        }
        Await.result(f, Duration.Inf)

        waitForCollection(1)
      }
    }
  }
}

object CachingSpec {

  private val cleaner = Cleaner.create()
  private val trackingGeneration = new AtomicInteger(0)
  private val collectedByGeneration = TrieMap.empty[Int, AtomicInteger]

  private def generationCounter(generation: Int): AtomicInteger = {
    collectedByGeneration.getOrElseUpdate(generation, new AtomicInteger(0))
  }

  startTracking()

  def startTracking(): Unit = {
    generationCounter(trackingGeneration.incrementAndGet()).set(0)
  }

  def count: Int = generationCounter(trackingGeneration.get()).get()

  def waitForCollection(expected: Int, timeoutMillis: Long = 5000L): Unit = {
    val deadlineNanos = System.nanoTime() + timeoutMillis * 1000000L

    while (count < expected && System.nanoTime() < deadlineNanos) {
      System.gc()
      Thread.sleep(50)
    }

    assert(count == expected)
  }

  private final class CleanupAction(generation: Int) extends Runnable {
    override def run(): Unit = {
      generationCounter(generation).incrementAndGet()
    }
  }

  case class CacheTestData(s: String = "") {
    cleaner.register(this, new CleanupAction(trackingGeneration.get()))
  }
}
