package org.shapesafe.graph.commons.util

import org.shapesafe.graph.commons.testlib.BaseSpec

class SingletonSummonerSpec extends BaseSpec {

  it("without") {
    object S0 extends AnyRef

    shouldNotCompile(
      "implicitly[S0.type]"
    )
  }

  it("with") {
    object S1 extends SingletonSummoner

    implicitly[S1.type]
  }
}
