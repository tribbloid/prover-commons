package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class ConversionPartSpec extends BaseSpec {

  describe("defined in object") {

    import ConversionPartSpec.H1.*
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

    import ConversionPartSpecFixture.*
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

}

object ConversionPartSpec {

  trait H1 extends HasConversionPart {

    case class A(value: Int)
    case class B(value: Int)
    case class C(value: Int)
    case class D(value: Int)
    case class E(value: Int)
    case class F(value: Int)

    // Direct conversions
    implicit def aToB: ConversionPart[A, B] = (v: A) => B(v.value)

    implicit val bToC: ConversionPart[B, C] = (v: B) => C(v.value + 1)

    implicit lazy val cToD: ConversionPart[C, D] = (v: C) => D(v.value + 2)

    implicit val dToE: ConversionPart[D, E] = (v: D) => E(v.value + 3)

    implicit val eToF: ConversionPart[E, F] = (v: E) => F(v.value + 4)
  }
  object H1 extends H1
}
