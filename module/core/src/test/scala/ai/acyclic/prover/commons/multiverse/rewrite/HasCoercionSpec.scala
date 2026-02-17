package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.verification.Verify

import scala.language.implicitConversions

class HasCoercionSpec extends BaseSpec {

  describe("defined in object") {

    import HasCoercionSpec.H1.*
    val a = A(1)

    it("can cast directly") {
      a: B
    }

    it("can cast through chain") {
      a: C
      a: D
      a: E
      a: F
    }
  }

  describe("defined in package") {

    import CoercionSpecFixture.*
    val a = A(1)

    it("can cast directly") {
      a: B
    }

    it("can cast through chain") {
      a: C
      a: D
      a: E
      a: F
    }
  }

  describe("Coerced mixed with Coercion") {

    import HasCoercionSpec.H2.*

    val a = A(1)

    it("can cast through Lemma B chain") {
      // A -> B -> C
      val b: B = a
      val cFromB: C = b // This triggers direct implicit conversion
      val cFromA: C = a // Chained Lemma B
      assert(cFromA.b.v == 2)
    }

    it("can cast through mixed chain") {
      // A -> B -> C -> D -> E
      val d: D = a
      assert(d.v == 4)

      val e: E = a
      assert(e.v == 7)
    }

  }

  describe("Coersion can be summoned") {

    import HasCoercionSpec.H2.*

    it("from Coerced") {

      implicitly[B <%< C]
    }

    it("from subtyping") {

      implicitly[D1 <%< D]
    }

    it("NOT from any implicit function") {

      implicit def notCoercion(v: C): Int = 1

      Verify.typeError(
        "implicitly[C <%< Int]"
      )
    }

    it(".. explicitly") {

      implicit def notCoercion: C => Int = _ => 1

      Verify.typeError(
        "implicitly[C <%< Int]"
      )
    }

    it("NOT from any implicit class") {

      implicit class NotCoercion(v: C)

      Verify.typeError(
        "implicitly[C <%< NotCoercion]"
      )
    }

  }

}

object HasCoercionSpec {

  trait H1 extends HasCoercion {

    case class A(value: Int)
    case class B(value: Int)
    case class C(value: Int)
    case class D(value: Int)
    case class E(value: Int)
    case class F(value: Int)

    // Direct conversions
    implicit def aToB: Coercion[A, B] = (v: A) => B(v.value)

    implicit val bToC: Coercion[B, C] = (v: B) => C(v.value + 1)

    implicit lazy val cToD: Coercion[C, D] = (v: C) => D(v.value + 2)

    implicit val dToE: Coercion[D, E] = (v: D) => E(v.value + 3)

    implicit val eToF: Coercion[E, F] = (v: E) => F(v.value + 4)
  }
  object H1 extends H1

  trait H2 extends HasCoercion {

    case class A(v: Int)

    case class B(v: Int) extends Coerced
    // Lemma B: T => R where R <: Coerced
    implicit val aToB: A => B = (x: A) => B(x.v + 1)

    trait D { def v: Int }
    case class D1(v: Int) extends D
    case class E(v: Int)

    implicit class C(val b: B) extends Coerced
    // Implicit class already bprovides B -> C conversion method,
    // but Scala 2.13 is too weak to use it in chained summoning, the following line can be removed in Scala 3
    implicit val bToC: B => C = (x: B) => new C(x)

    // Lemma A: ConversionPart

    // Lemma A: ConversionPart
    // C -> D
    implicit val cToD: Coercion[C, D1] = (x: C) => D1(x.b.v + 2)

    // D -> E
    implicit val dToE: Coercion[D, E] = (x: D) => E(x.v + 3)
  }
  object H2 extends H2
}
