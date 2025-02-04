package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.debug.SrcDefinition

object TraceableSpec {

  val t1: Traceable = new Traceable {}

  case class T2()(
      implicit
      override val _definedAt: SrcDefinition
  ) extends Traceable

  val t2: T2 = T2()

  object T3 extends T2()
  T3

//  abstract class SAM(
//      implicit
//      val _definedAt: SrcDefinition
//  ) extends Traceable {
//    def apply(): Int
//  }
//
//  val sam1: SAM = { () => 1 }
//
//  object sam2 extends SAM {
//    override def apply(): Int = 1
//  }
}

class TraceableSpec extends BaseSpec {

  import TraceableSpec.*
  describe("of instances") {

    it("ad-hoc class") {
      t1.definedAt.AtLine.short.shouldBe("<unknown>:0")
    }

    it("class") {
      t2.definedAt.AtLine.short.shouldBe("TraceableSpec.scala:15")
    }

    it("object") {
      T3.definedAt.AtLine.short.shouldBe("TraceableSpec.scala:17")
    }

//    it("single-abstract method") {
//      sam1._definedAt.atLine.shouldBe("TraceableSpec.scala:21")
//    }

//    it("... extended") {
//      sam2.definedAt.atLine.shouldBe("TraceableSpec.scala:21")
//    }
  }
}
