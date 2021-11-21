package org.shapesafe.graph.commons.util.debug

import org.shapesafe.graph.commons.testlib.BaseSpec

class print_At_Spec extends BaseSpec {

  it("can identify line number") {

    val str = print_@.dryRun("abc")
    assert(str.contains(s"(${this.getClass.getSimpleName}.scala:"))
  }
}
