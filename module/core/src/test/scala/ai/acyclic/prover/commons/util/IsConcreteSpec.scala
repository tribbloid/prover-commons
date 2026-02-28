package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.verification.Verify

class IsConcreteSpec extends BaseSpec {

  trait BaseTrait
  class ConcreteClass extends BaseTrait
  object ConcreteObject extends BaseTrait

  describe("IsConcrete") {

    it("should materialize for concrete trait") {
      val isConcrete = implicitly[IsConcrete[BaseTrait]]
      assert(isConcrete != null)
    }

    it("should materialize for concrete class") {
      val isConcrete = implicitly[IsConcrete[ConcreteClass]]
      assert(isConcrete != null)
    }

    it("should materialize for String") {
      val isConcrete = implicitly[IsConcrete[String]]
      assert(isConcrete != null)
    }

    it("should fail to materialize for Nothing") {
      Verify.typeError(
        "implicitly[IsConcrete[Nothing]]"
      )
    }

    it("should fail to materialize for refined type") {
      Verify.typeError(
        "implicitly[IsConcrete[BaseTrait { type T = Int }]]"
      )
    }

    it("should fail to materialize for intersection type") {
      Verify.typeError(
        "implicitly[IsConcrete[BaseTrait with Product]]"
      )
    }
  }
}
