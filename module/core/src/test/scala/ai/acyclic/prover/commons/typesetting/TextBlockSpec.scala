package ai.acyclic.prover.commons.typesetting

import org.scalatest.funspec.AnyFunSpec

class TextBlockSpec extends AnyFunSpec {
  // keep using AnyFunSpec, may be migrated to another library

  // Helper method to normalize line endings for cross-platform compatibility
  private def normalizeLineEndings(s: String): String = s.replace("\r\n", "\n").replace("\r", "\n")

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
        normalizeLineEndings(untrimmed.trim.left_right.toString).===(
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
        normalizeLineEndings(untrimmed.trim.left_right.toString).===(
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

        normalizeLineEndings(untrimmed.trim.top_bottom.toString).===(
          """       /---\
            |      /  |  \
            |     /   |   \
            |    /----|----\""".stripMargin
        )
      }
    }

    it("block") {

      assert {

        normalizeLineEndings(untrimmed.trim.block.toString).===(
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
        normalizeLineEndings(
          untrimmed
            .zipRight(untrimmed.trim.block)
            .toString
        )
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
        normalizeLineEndings(
          untrimmed
            .zipBottom(untrimmed.trim.block)
            .toString
        )
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
