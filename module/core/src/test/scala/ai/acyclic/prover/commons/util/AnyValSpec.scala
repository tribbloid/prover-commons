package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.verification.Verify

object AnyValSpec {}

class AnyValSpec extends BaseSpec {

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

    it(
      "AnyVal can be used as a structural refinee type, but values don't conform without boxing/reflection in standard Scala, and often fail implicitly"
    ) {
      // In Scala, you can define a structural type
      type RefinedAnyVal = AnyVal { def toHexString: String }

      // Int has toHexString, but checking if it conforms structurally without implicit conversion
      // `AnyVal` itself does not declare `toHexString`, it's added via implicit `RichInt` or boxing.
      // E.g., a primitive int does not directly conform to this structural type natively in a way that doesn't cause overhead or fail.
      Verify.typeError(
        "new RefinedAnyVal {}"
      )
    }

    it(
      "even with an explicit value class, it cannot simply fulfill a structural refinement seamlessly without boxing"
    ) {
      """
        |class MyVal(val i: Int) extends AnyVal {
        |  def customMethod: String = i.toString
        |}
        |""".stripMargin // compiles on its own

      // Try to assign it to a refined AnyVal
      Verify.typeError(
        "class MyVal(val i: Int) extends AnyVal { def customMethod: String = i.toString }; type Ref = AnyVal { def customMethod: String }; val y: Ref = new MyVal(5)"
      )
    }

  }
}
