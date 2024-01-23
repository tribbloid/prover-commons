package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

object DefinedAtMixinSpec {

  val t1: DefinedAtMixin =
    new DefinedAtMixin {}

  case class T2() extends DefinedAtMixin
  val t2: T2 =
    T2()

  object T3 extends T2()
  T3

  trait SAM1 extends DefinedAtMixin {
    def apply(): Int
  }

  val sam1: SAM1 =
    () => 1
}

class DefinedAtMixinSpec extends BaseSpec {

  import DefinedAtMixinSpec._
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
