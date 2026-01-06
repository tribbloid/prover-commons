package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class ConversionPartSpec extends BaseSpec {

  import ConversionPartSpec.*

  describe("ConversionPart") {

    describe("forward chaining") {

      it("chains two conversions forward") {

        val a = A(42)

        val c: C = a
        assert(c.value == 43)
      }

      it("chains three conversions forward") {

        val a = A(99)

        val d: D = a
        assert(d.value == 102)
      }

      it("can be used in expressions requiring the target type") {

        val a = A(123)

        def expectC(c: C): Int = c.value

        val result = expectC(a)
        assert(result == 124)
      }
    }

    describe("backward chaining") {

      it("chains two conversions backward") {

        val b = B(77)

        val d: D = b
        assert(d.value == 80)
      }

      it("chains three conversions backward") {

        val c = C(55)

        val d: D = c
        assert(d.value == 57)
      }
    }

    describe("bidirectional chaining") {

      it("chains from A to D through B and C") {

        val a = A(1000)

        val d: D = a
        assert(d.value == 1003)
      }

      it("chains from A to C") {

        val a = A(777)

        val c: C = a
        assert(c.value == 778)
      }
    }

    describe("implicit resolution") {

      it("resolves intermediate types implicitly") {

        val a = A(42)

        val c: C = a
        val cValue: Int = c.value

        assert(cValue == 43)
      }

      it("works with type inference") {

        val a = A(256)

        val result = (a: C).value
        assert(result == 257)
      }
    }

    describe("with value transformations") {

      it("applies transformations through the chain") {

        val a = A(10)

        val d: D = a
        assert(d.value == 13)
      }
    }
  }
}

object ConversionPartSpec {

  case class A(value: Int)
  case class B(value: Int)
  case class C(value: Int)
  case class D(value: Int)

  // Direct conversions
  implicit val aToB: ConversionPart[A, B] = new ConversionPart[A, B] {
    override def normalise(v: A): B = B(v.value)
  }

  implicit val bToC: ConversionPart[B, C] = new ConversionPart[B, C] {
    override def normalise(v: B): C = C(v.value + 1)
  }

  implicit val cToD: ConversionPart[C, D] = new ConversionPart[C, D] {
    override def normalise(v: C): D = D(v.value + 2)
  }

  // Chained conversions (using the chain method from ConversionPart companion)
  // Scala 2's implicit view search requires these to be explicitly defined as vals
  implicit val aToC: ConversionPart[A, C] = ConversionPart.chain(aToB, bToC)
  implicit val bToD: ConversionPart[B, D] = ConversionPart.chain(bToC, cToD)
  implicit val aToD: ConversionPart[A, D] = ConversionPart.chain(aToC, cToD)
}
