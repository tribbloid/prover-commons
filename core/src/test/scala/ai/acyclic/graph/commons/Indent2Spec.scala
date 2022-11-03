package ai.acyclic.graph.commons

class Indent2Spec extends TreeLikeSpec {

  override lazy val format = TreeFormat.Indent2

  describe("toString") {
    it("supports nodes each with 1 line str") {

      tree1.treeString shouldBe
        """
          |-+ aaa
          | !-+ bbb
          | : !-- ddd
          | !-- ccc
          |""".stripMargin

    }

    it("... or not") {

      // TODO: this is wrong! second line is not flushed
      tree2.treeString shouldBe
        """
          |-+ aaa
          | : %%%%%
          | !-+ bbb
          | : : %%%%%
          | : !-- ddd
          | :     %%%%%
          | !-- ccc
          |     %%%%%
          |""".stripMargin

    }
  }
}
