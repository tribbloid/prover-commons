package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

class InhabitedSpec extends BaseSpec {

  it("should be covariant") {
    implicitly[Inhabited[Tuple1[Int]] <:< Inhabited[Product]]

    def assuming[X, Y <: X] = {
      implicitly[Inhabited[Y] <:< Inhabited[X]]
      implicitly[Inhabited[Tuple1[Y]] <:< Inhabited[Tuple1[X]]]
    }
  }
}
