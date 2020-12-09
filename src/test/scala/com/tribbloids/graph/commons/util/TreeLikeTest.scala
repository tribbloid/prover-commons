package com.tribbloids.graph.commons.util

import org.scalatest.FunSpec

class TreeLikeTest extends FunSpec {

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

      assert(
        tree.toString ===
        """
            |+ aaa
            |:-+ bbb
            || :-- ddd
            |:-- ccc
            |""".stripMargin.trim
      )
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

      assert(
        tree.toString ===
          """
            |+ aaa
            || %%%%%
            |:-+ bbb
            || | %%%%%
            || :-- ddd
            ||     %%%%%
            |:-- ccc
            |    %%%%%
            |""".stripMargin.trim
      )
    }
  }
}
