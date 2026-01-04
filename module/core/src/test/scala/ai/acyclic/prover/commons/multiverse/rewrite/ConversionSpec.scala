package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class ConversionSpec extends BaseSpec {

  describe("implicitly invoked") {

    it("directly") {

      case class A()
      object A extends Conversion[A, Int] {
        override def normalise(v: A): Int = 1
      }

      val a: A = A()
      a: Int
    }

    it("indirectly") {

      case class A[T]() {}
      object A {
        implicit def convert[T]: Conversion[A[T], Int] = (_: A[T]) => 1
      }

      val a: A[String] = A()
      a: Int
    }
  }
}
