package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.viz.format.Formats0.TypeInfo
import ai.acyclic.prover.commons.viz.format.Formats1.DeAlias

class TypeFormatSpec extends BaseSpec {

  it("~") {

    val v1 = TypeInfo.DeAlias

    val v2 = DeAlias(TypeInfo)

    assert(v1 == v2)
  }
}
