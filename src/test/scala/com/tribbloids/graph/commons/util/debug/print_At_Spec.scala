package com.tribbloids.graph.commons.util.debug

import com.tribbloids.graph.commons.testlib.BaseSpec

class print_At_Spec extends BaseSpec {

  it("can identify line number") {

    val str = print_@.dryRun("abc")
    assert(str.contains(s"(${this.getClass.getSimpleName}.scala:"))
  }
}
