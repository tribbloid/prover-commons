package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.verification.Verify

final class Meter(val value: Int) extends AnyVal

class MayExistSpec extends BaseSpec {

  trait OpenTrait
  class OpenClass extends OpenTrait
  class AnotherClass
  final class FinalClass
  object Singleton extends OpenTrait

  describe("MayExist") {

    it("should be covariant and be a supertype of Inhabited") {
      implicitly[MayExist[OpenClass] <:< MayExist[OpenTrait]]
      implicitly[Inhabited[Int] <:< MayExist[Int]]

      def widen[X, Y <: X](value: MayExist[Y]): MayExist[X] = value

      assert(widen[OpenTrait, OpenClass](implicitly[MayExist[OpenClass]]) != null)
    }

    it("should materialize for inhabited plain types") {
      assert(implicitly[MayExist[OpenTrait]] != null)
      assert(implicitly[MayExist[OpenClass]] != null)
      assert(implicitly[MayExist[String]] != null)
      assert(implicitly[MayExist[Int]] != null)
      assert(implicitly[MayExist[Meter]] != null)
      assert(implicitly[MayExist[Array[String]]] != null)
      assert(implicitly[MayExist[Singleton.type]] != null)
    }

    it("should materialize for refinements of extendable types") {
      assert(implicitly[MayExist[OpenTrait { type Out = Int }]] != null)
      assert(implicitly[MayExist[OpenClass { def extra: Int }]] != null)
    }

    it("should materialize when a final type already satisfies the refinement") {
      assert(implicitly[MayExist[String with CharSequence]] != null)
      assert(implicitly[MayExist[Array[Int] with Cloneable]] != null)
    }

    it("should fail for Nothing") {
      Verify.typeError(
        "implicitly[MayExist[Nothing]]"
      )
    }

    it("should fail for proper subtypes of final types") {
      Verify.typeError(
        "implicitly[MayExist[FinalClass { type Out = Int }]]"
      )
      Verify.typeError(
        "implicitly[MayExist[String with Product]]"
      )
      Verify.typeError(
        "implicitly[MayExist[Meter { type Out = Int }]]"
      )
    }

    it("should fail for incompatible class intersections") {
      Verify.typeError(
        "implicitly[MayExist[OpenClass with AnotherClass]]"
      )
    }
  }
}
