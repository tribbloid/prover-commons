package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class ConversionSpec extends BaseSpec {

  describe("invoke implicit") {

    it("val") {

      case class A() {}
      object A {
        implicit val convert: Conversion[A, Int] = (_: A) => 1
      }

      val a: A = A()
      a: Int
    }

    it("def") {

      case class A[T]() {}
      object A {
        implicit def convert[T]: Conversion[A[T], Int] = (_: A[T]) => 1
      }

      val a: A[String] = A()
      a: Int
    }

    it("lazy val") {

      case class A() {}
      object A {
        implicit lazy val convert: Conversion[A, Int] = (_: A) => 1
      }

      val a: A = A()
      a: Int
    }
  }
}
