package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class ConversionPartSpec extends BaseSpec {

  import ConversionPartSpec.H1.*
  val a = A(1)

  it("can cast directly") {
    a: B
  }

  it("can cast through chain") {
    a: C
    a: D
  }
}

object ConversionPartSpec {

  object H1 extends HasConversionPart {

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
  }

}
