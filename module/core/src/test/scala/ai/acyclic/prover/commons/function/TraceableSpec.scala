package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec

object TraceableSpec {

  val t1: Traceable = new Traceable {}

  case class T2() extends Traceable
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
      t1.definedAt.atLine.shouldBe("TraceableSpec.scala:9")
    }

    it("class") {
      t2.definedAt.atLine.shouldBe("TraceableSpec.scala:12")
    }

    it("object") {
      T3.definedAt.atLine.shouldBe("TraceableSpec.scala:14")
    }

//    it("single-abstract method") {
//      sam1._definedAt.atLine.shouldBe("TraceableSpec.scala:21")
//    }

//    it("... extended") {
//      sam2.definedAt.atLine.shouldBe("TraceableSpec.scala:21")
//    }
  }
}
