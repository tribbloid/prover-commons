package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.HNil

object T1Spec {
  import shapeless.::

  // Single Abstract Method definition
  val samFn: T1.Function[Int :: HNil, String] = { v =>
    val _ = v._1 // uses implicits

    val tt: Tuple1[Int] = v.asTuple // use dynamic selector

    "" + tt._1
  }
}

class T1Spec extends BaseSpec {

  import T1Spec._

  describe("toString") {

    it("function") {
      samFn.toString.shouldBe(
        "(defined at: T1Spec.scala:10)"
      )
    }
  }

  describe("currying") {

    it("function") {
      assert(samFn(1) == "1")

      val fn2 = samFn.curry(1)
      fn2.toString.shouldBe(
        "(defined at: T1Spec.scala:10).curry(1)"
      )
      assert(fn2() == "1")
    }
  }

//  describe("Single Abstract Method definition for") {
//
//    it("function") {
//
//      val fn1: Tier1.Function[Int :: HNil, String] = { v =>
//        val _ = v._1 // uses implicits
//
//        val tt: Tuple1[Int] = v.asTuple // use dynamic selector
//
//        "" + tt._1
//      }
//    }
//  }
}
