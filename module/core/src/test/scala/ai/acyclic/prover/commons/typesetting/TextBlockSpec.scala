package ai.acyclic.prover.commons.typesetting

import org.scalatest.funspec.AnyFunSpec

class TextBlockSpec extends AnyFunSpec {
  // keep using AnyFunSpec, may be migrated to another library

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

  describe("Trim") {

    it("horizontally") {

      assert {
        untrimmed.trim.left_right.toString.===(
          """
            |
            |
            |   /---\
            |  /  |  \
            | /   |   \
            |/----|----\""".stripMargin
        )
      }

      assert {
        untrimmed.trim.left_right.toString.===(
          """
            |
            |
            |   /---\
            |  /  |  \
            | /   |   \
            |/----|----\""".stripMargin
        )
      }

    }

    it("vertically") {

      assert {

        untrimmed.trim.top_bottom.toString.===(
          """       /---\
            |      /  |  \
            |     /   |   \
            |    /----|----\""".stripMargin
        )
      }
    }

    it("block") {

      assert {

        untrimmed.trim.block.toString.===(
          """   /---\
            |  /  |  \
            | /   |   \
            |/----|----\""".stripMargin
        )
      }
    }
  }

  describe("zip") {
    it("right") {

      assert {
        untrimmed
          .zipRight(untrimmed.trim.block)
          .toString
          .===(
            """                  /---\
              |                 /  |  \
              |                /   |   \
              |       /---\   /----|----\
              |      /  |  \
              |     /   |   \
              |    /----|----\""".stripMargin
          )
      }
    }

    it("bottom") {

      assert {
        untrimmed
          .zipBottom(untrimmed.trim.block)
          .toString
          .===(
            """
              |
              |
              |       /---\
              |      /  |  \
              |     /   |   \
              |    /----|----\
              |   /---\
              |  /  |  \
              | /   |   \
              |/----|----\""".stripMargin
          )
      }
    }
  }
}
