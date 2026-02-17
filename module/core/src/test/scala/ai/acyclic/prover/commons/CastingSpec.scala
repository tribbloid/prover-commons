package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.Casting.{<:<, CanCast}

class CastingSpec extends BaseSpec {

  import CastingSpec.* // imports HasCoercion implicits

  describe("Casting.<:<") {

    it("can trigger 1-step coercion") {
      val impl = new Impl
      val a: A = impl
      val _: B = a
      val _: A & B = a
    }

    it("can trigger 2-step coercion") {
      val impl = new Impl
      val a: A = impl
      val _: C = a
      val _: B & C = a
      val _: A & B & C = a
    }

    it("can trigger 3-step coercion") {
      val impl = new Impl
      val a: A = impl
      val _: D = a
      val _: C & D = a
      val _: B & C & D = a
      val _: A & B & C & D = a
    }
  }
}

object CastingSpec {

  trait A
  trait B
  trait C
  trait D

  class Impl extends A with B with C with D

  implicit val lemma1: A <:< B = new CanCast[A, B]()
  implicit val lemma2: B <:< C = new CanCast[B, C]()
  implicit val lemma3: C <:< D = new CanCast[C, D]()

}
