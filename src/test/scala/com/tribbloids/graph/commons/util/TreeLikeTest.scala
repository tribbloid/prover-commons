package com.tribbloids.graph.commons.util

import com.tribbloids.graph.commons.testlib.BaseSpec

class TreeLikeTest extends BaseSpec {

  import TreeLike._

  describe("toString") {
    it("supports nodes each with 1 line str") {

      val tree = Str(
        "aaa",
        Seq(
          Str(
            "bbb",
            Seq(
              Str("ddd")
            )
          ),
          Str(
            "ccc"
          )
        )
      )

      tree.treeString shouldBe
        """
            |-+ aaa
            | !-+ bbb
            | : !-- ddd
            | !-- ccc
            |""".stripMargin.trim

    }

    it("... or not") {

      val tree = Str(
        "aaa\n%%%%%",
        Seq(
          Str(
            "bbb\n%%%%%",
            Seq(
              Str("ddd\n%%%%%")
            )
          ),
          Str(
            "ccc\n%%%%%"
          )
        )
      )

      tree.treeString shouldBe
        """
            |-+ aaa
            | :%%%%%
            | !-+ bbb
            | : :%%%%%
            | : !-- ddd
            | :    %%%%%
            | !-- ccc
            |    %%%%%
            |""".stripMargin.trim

    }
  }
}
