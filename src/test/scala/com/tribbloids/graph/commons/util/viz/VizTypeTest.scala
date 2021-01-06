package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.testlib.BaseSpec
import shapeless.Witness

class VizTypeTest extends BaseSpec {

  import VizTypeTest._

  it("String") {

    VizType[String].toString.shouldBe(
      """
        |-+ String .................................................................................................................. [0]
        | !-+ CharSequence
        | : !-+ Object
        | :   !-- Any
        | !-+ Comparable[String]
        | : !-+   [ 1 ARG ] :
        | :   !-- String .................................................................................................................. [0]
        | !-- java.io.Serializable
        |""".stripMargin.trim
    )
  }

  it("Singleton") {

    // TODO: doesn't work! why?
    // TODO: may use type pimp pattern
//    VizType[ww.T].toString.shouldBe()
//
//    VizType.infer(ss).toString.shouldBe()
  }
}

object VizTypeTest {

  val ww: Witness.Lt[Int] = Witness(3)

  val ss = ww.value
}
