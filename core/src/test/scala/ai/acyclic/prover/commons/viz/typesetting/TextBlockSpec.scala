package ai.acyclic.prover.commons.viz.typesetting

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.typesetting.TextBlock

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

      untrimmed.trim.left_right.toString.shouldBe(
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

      untrimmed.trim.top_bottom.toString.shouldBe(
        """       /---\
          |      /  |  \
          |     /   |   \
          |    /----|----\
          |""".stripMargin,
        trim = identity
      )
    }

    it("block") {

      untrimmed.trim.block.toString.shouldBe(
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
