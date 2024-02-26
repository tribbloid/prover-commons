package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class HasConversionPartSpec extends BaseSpec {

  describe("defined in object") {

    import HasConversionPartSpec.H1.*
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

  describe("PartiallyConverted mixed with ConversionPart") {

    import HasConversionPartSpec.H2
    import H2.*
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
      val d: D = a // Lemma B -> Lemma B -> Lemma A
      assert(d.v == 4)

      val e: E = a // Lemma B -> Lemma B -> Lemma A -> Lemma A
      assert(e.v == 7)
    }
  }

}

object HasConversionPartSpec {

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

  trait H2 extends HasConversionPart {

    case class A(v: Int)

    case class B(v: Int) extends PartiallyConverted
    // Lemma B: T => R where R <: PartiallyConverted
    implicit val aToB: A => B = (x: A) => B(x.v + 1)

    case class D(v: Int)
    case class E(v: Int)

    implicit class C(val b: B) extends PartiallyConverted
    // Implicit class already bprovides B -> C conversion method,
    // but Scala 2.13 is too weak to use it in chained summoning, the following line can be removed in Scala 3
    implicit val bToC: B => C = (x: B) => new C(x)

    // Lemma A: ConversionPart

    // Lemma A: ConversionPart
    // C -> D
    implicit val cToD: ConversionPart[C, D] = (x: C) => D(x.b.v + 2)

    // D -> E
    implicit val dToE: ConversionPart[D, E] = (x: D) => E(x.v + 3)
  }
  object H2 extends H2
}
