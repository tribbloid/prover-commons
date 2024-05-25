package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

object SrcTraceableSpec {

  val t1: SrcTraceable =
    new SrcTraceable {}

  case class T2() extends SrcTraceable
  val t2: T2 =
    T2()

  object T3 extends T2()
  T3

  trait SAM1 extends SrcTraceable {
    def apply(): Int
  }

  val sam1: SAM1 =
    () => 1
}

class SrcTraceableSpec extends BaseSpec {

  import SrcTraceableSpec._
  describe("of instances") {

    it("ad-hoc class") {
      t1.definedAt.atLine.shouldBe("DefinedAtMixinSpec.scala:8")
    }

    it("class") {
      t2.definedAt.atLine.shouldBe("DefinedAtMixinSpec.scala:12")
    }

    it("object") {
      T3.definedAt.atLine.shouldBe("DefinedAtMixinSpec.scala:14")
    }

    it("single-abstract method") {
      sam1.definedAt.atLine.shouldBe("DefinedAtMixinSpec.scala:22")
    }
  }
}
