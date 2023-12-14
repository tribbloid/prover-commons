package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

class SectionSpec extends BaseSpec {

  describe("for instances of S[Int] <: Section") {

    ignore("dependent types of each are also identical") {}
  }

}

object SectionSpec {

  object For {
    def apply[T] = new For[T]
  }

  final class For[T] extends Section[For[T]] {

    case class Fn[R]()

    locally {
      val _: Fn[Int] = ??? : For[T]#Fn[Int] // works because of the implicit conversion
    }
  }
}
