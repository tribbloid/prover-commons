package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.testlib.BaseSpec

class print_At_Spec extends BaseSpec {

  it("can identify line number") {

    val str = print_@.wrapInfo("abc")
    assert(str.contains(s"(${this.getClass.getSimpleName}.scala:"))
  }
}
