package ai.acyclic.prover.commons.viz.text

import ai.acyclic.prover.commons.testlib.BaseSpec

class TextBlockSpec extends BaseSpec {

  describe("Trim") {

    val untrimmed = TextBlock(
      s"""
         |
         |
         |       /---\\
         |      /  |  \\
         |     /   |   \\
         |    /----|----\\
         |
         |
         |
         |""".stripMargin
    )

    it("horizontally") {

      untrimmed.Trim.left_right.toString.shouldBe(
        """
          |
          |
          |   /---\
          |  /  |  \
          | /   |   \
          |/----|----\
          |""".stripMargin,
        trim = identity
      )
    }

    it("vertically") {

      untrimmed.Trim.top_bottom.toString.shouldBe(
        """       /---\
          |      /  |  \
          |     /   |   \
          |    /----|----\
          |""".stripMargin,
        trim = identity
      )
    }

    it("block") {

      untrimmed.Trim.block.toString.shouldBe(
        """   /---\
          |  /  |  \
          | /   |   \
          |/----|----\
          |""".stripMargin,
        trim = identity
      )
    }
  }
}
