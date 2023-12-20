package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.Symbolic.:=>
import ai.acyclic.prover.commons.testlib.BaseSpec

object T1Spec {
  import Symbolic._

  // Single Abstract Method definition
  lazy val _fn: Int :=> String = { v =>
    val _ = v._1 // uses implicits

    val tt: Tuple1[Int] = v.asTuple // use dynamic selector

    "" + tt._1
  }

  lazy val _morphism: List :|~> Option = new (List :|~> Option) {

    override def specific[T]: List[T] :=> Option[T] = { v =>
      v.headOption
    }
  }

  object _poly extends Poly1 {

    implicit def int: Case[Int :=> Int] = {

      val at1 = at[Int :=> Int]
      at1.apply(v => v.unbox1 + 1)
    }

    implicit def str: _poly.Case[String :=> String] =
      at[String :=> _](v => v.unbox1 + "1")
  }
}

class T1Spec extends BaseSpec {

  import T1Spec._

  lazy val fn: Int :=> String = {

    _fn.toString.shouldBe(
      "<defined at: T1Spec.scala:10>"
    )
    _fn
  }

  lazy val poly: T1Spec._poly.type = {
    _poly.toString.shouldBe(
      "<defined at: T1Spec.scala:50>"
    )
    _poly
  }

  describe("application & currying") {

    it("function") {
      assert(fn(1) == "1")

      val fn2 = fn.curry(1)
      fn2.toString.shouldBe(
        "<defined at: T1Spec.scala:10>.curry(1)"
      )
      assert(fn2() == "1")
    }

    it("poly") {

      assert(poly.apply(1) == 2)
      assert(poly.apply("1") == "11")
    }
  }

  describe("chain") {

    it("function") {}
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
