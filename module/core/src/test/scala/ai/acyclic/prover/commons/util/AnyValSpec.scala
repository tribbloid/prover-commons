package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

import ai.acyclic.prover.commons.verification.Verify

object AnyValSpec {

  class MyVal(val i: Int) extends AnyVal {

    def customMethod: String = i.toString
  }

  // Refining the type definition compiles just fine
  type Ref = MyVal { def customMethod: String }
  val y: Ref = new MyVal(5)

  trait MustBeVal {
    self: AnyVal =>
  }

  class MyVal2(val u: Unit) extends AnyVal with MustBeVal
}

class AnyValSpec extends BaseSpec {

  import ai.acyclic.prover.commons.util.AnyValSpec.MyVal

  describe("AnyVal limitations") {

    it("cannot be refined anonymously with new AnyVal { ... }") {
      Verify.typeError(
        "new AnyVal { def customMethod: Int = 42 }"
      )
    }

    it("custom value classes cannot have multiple constructor parameters") {
      Verify.typeError(
        "class InvalidValueClass(val a: Int, val b: Int) extends AnyVal"
      )
    }

    it("custom value classes cannot define `val` or `var` inside the body") {
      Verify.typeError(
        "class InvalidValueClass(val a: Int) extends AnyVal { val additionalVal = 42 }"
      )
      Verify.typeError(
        "class InvalidValueClass(val a: Int) extends AnyVal { var additionalVar = 42 }"
      )
    }

    it("custom value classes cannot be extended") {
      Verify.typeError(
        "class ValidValueClass(val a: Int) extends AnyVal; class SubClass(a: Int) extends ValidValueClass(a)"
      )
    }

    it("subclass of AnyVal can be refined in type definition, but value cannot be defined anonymously") {
      // Trying to instantiate an anonymous subclass of MyVal
      Verify.typeError(
        "new MyVal(5) {}"
      )

      Verify.typeError(
        "type Ref2 = MyVal { def customMethod: String }; val y: Ref2 = new MyVal(5) { override def customMethod: String = \"\" }"
      )
    }

  }
}
