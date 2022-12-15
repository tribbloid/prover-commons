package ai.acyclic.prover.commons.reflect.format

import ai.acyclic.prover.commons.reflect.format.Formats0.TypeInfo
import ai.acyclic.prover.commons.reflect.format.Formats1.DeAlias
import ai.acyclic.prover.commons.testlib.BaseSpec

class TypeFormatSpec extends BaseSpec {

  it("~") {

    val v1 = TypeInfo.DeAlias

    val v2 = TypeInfo ~ DeAlias

    assert(v1 == v2)
  }
}
