package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec

object TraceableSpec {

  import Traceable._

  val t1: BySrc =
    new BySrc {}

  case class T2() extends BySrc
  val t2: T2 =
    T2()

  object T3 extends T2()
  T3

  trait SAM1 extends BySrc {
    def apply(): Int
  }

  val sam1: SAM1 =
    () => 1
}

class TraceableSpec extends BaseSpec {

  import TraceableSpec._
  describe("of instances") {

    it("ad-hoc class") {
      t1.definedAt.atLine.shouldBe("SrcTraceableSpec.scala:8")
    }

    it("class") {
      t2.definedAt.atLine.shouldBe("SrcTraceableSpec.scala:12")
    }

    it("object") {
      T3.definedAt.atLine.shouldBe("SrcTraceableSpec.scala:14")
    }

    it("single-abstract method") {
      sam1.definedAt.atLine.shouldBe("SrcTraceableSpec.scala:22")
    }
  }
}
