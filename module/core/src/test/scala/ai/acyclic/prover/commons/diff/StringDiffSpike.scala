package ai.acyclic.prover.commons.diff

import ai.acyclic.prover.commons.testlib.BaseSpec

class StringDiffSpike extends BaseSpec {

  val left: String =
    """
      |c1
      |  c2
      |    c3
      |  c4
      |""".stripMargin

  val right: String =
    """
      |c1
      |  d2
      |    c3
      |    c4
      |""".stripMargin

  ignore("diff display in IDE") { // Only enable if IDE broke or change convention

    it("using Predef.assert()") {
      val diff = StringDiff(Some(left), Some(right))

      diff.assert()
    }

    it("using Scalatest.assert()") {
      val diff = StringDiff(Some(left), Some(right))

      diff.assert(usingFn = assert(_, _))
    }
  }
}
