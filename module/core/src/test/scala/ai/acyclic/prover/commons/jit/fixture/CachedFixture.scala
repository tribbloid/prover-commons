package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.Hom.{Const, Fn}
import ai.acyclic.prover.commons.jit.eval.Args.{><:, T0}
import java.util.concurrent.atomic.AtomicInteger

object CachedFixture {

  def createCounterFn(): (AtomicInteger, Fn[Int ><: T0, String]) = {
    val counter = new AtomicInteger(0)
    val fn: Fn[Int ><: T0, String] = { (v: Int) =>
      counter.incrementAndGet()
      s"value:$v"
    }
    (counter, fn)
  }

  def createCachedFn(): (AtomicInteger, Fn.CachedImpl[Int ><: T0, String]) = {
    val (counter, fn) = createCounterFn()
    (counter, fn.cached())
  }
}
