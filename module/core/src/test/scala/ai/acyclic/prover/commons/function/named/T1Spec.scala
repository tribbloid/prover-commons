package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.testlib.BaseSpec

object T1Spec {}

class T1Spec extends BaseSpec {

  import Fixtures._
  import ai.acyclic.prover.commons.function.PreDef.Named._

  lazy val fn: Int :=> String = {

    Fixtures._fn0.toString.shouldBe(
      "<defined at: FnFixture.scala:8>"
    )
    _fn0
  }

  lazy val morphism: List :|~> Option = {
    _morphism.toString.shouldBe(
      "<defined at: MorphismFixture.scala:7>"
    )
    _morphism
  }

  lazy val poly: Fixtures._poly.type = {
    _poly.toString.shouldBe(
      "<defined at: Fixtures.scala:3>"
      // TODO: this cannot be improved as Poly can only be defined as object
    )
    _poly
  }

  describe("application & currying") {

    it("function") {
      assert(fn(1) == "1")

      val fn2 = fn.curry(1)
      fn2.toString.shouldBe(
        "curry(1) <~ (<defined at: FnFixture.scala:8>)"
      )
      assert(fn2() == "1")
    }

    it("morphism") {
      assert(morphism(List(1, 2, 3)) == Some(1))
      assert(morphism(List.empty) == None)
    }

    it("poly") {

      assert(poly.apply(1) == 2)
      assert(poly.apply("1") == "11")
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
