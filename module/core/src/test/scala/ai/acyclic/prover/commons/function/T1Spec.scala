package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec

object T1Spec {}

class T1Spec extends BaseSpec {

  import Fixtures._
  import ai.acyclic.prover.commons.function.Symbolic._

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
        "<defined at: FnFixture.scala:8>.curry(1)"
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

  describe("chain") {

    it("function") {

      val chainedSelf = Fixtures._fn1.andThen(_fn1)
      assert(chainedSelf(1) == 3)

      val chainedOthers = fn.andThen { v =>
        v.value1 + "b"
      }

      val chainedOthers2 = fn.andThen2 { v =>
        v + "b"
      }
      assert(chainedOthers(1) == "1b")
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
