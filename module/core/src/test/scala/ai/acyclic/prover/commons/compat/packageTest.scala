package ai.acyclic.prover.commons.compat

import ai.acyclic.prover.commons.compat.NamedTupleX.:=
import ai.acyclic.prover.commons.compat.TupleX.{*:, T0}
import ai.acyclic.prover.commons.testlib.BaseSpec

object packageTest {

  val tx = TupleX.of(1, "a")
  val ntx = (Key["a"] := 1) *: (Key["b"] := "x") *: T0
}

class packageTest extends BaseSpec {

  describe("define") {

    it("Tuple") {

      val t0 = 1 *: "a" *: T0
      t0: Int *: String *: T0

      val t1 = TupleX.of(1, "a")
      t1: Int *: String *: T0
      t1: 1 *: "a" *: T0 // TODO: how does this work?

      val t2 = TupleX.ofNarrow(1, "a")
      t2: Int *: String *: T0
      t2: 1 *: "a" *: T0
    }

    it("NamedTuple") {

      val t0 = (Key["a"] := 1) *: (Key["b"] := "x") *: T0
      t0: ("a" := Int) *: ("b" := String) *: T0

      val t1 = NamedTupleX.of(a = 1, b = "x")
      t1: ("a" := Int) *: ("b" := String) *: T0

      import shapeless.record.recordOps

      val aa = t1.record.a
      aa: Int

      assert(aa == 1)
    }
  }
}
