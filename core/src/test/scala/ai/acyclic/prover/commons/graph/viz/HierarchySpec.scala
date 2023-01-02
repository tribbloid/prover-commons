package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.TreeFixture

class HierarchySpec extends TreeFixture {

  describe(Hierarchy.Indent2.productPrefix) {
    lazy val format = Hierarchy.Indent2

    describe("treeString") {
      it("supports nodes each with 1 line str") {

        tree1.showHierarchy.treeString shouldBe
          """
            |-+ aaa
            | !-+ bbb
            | : !-- ddd
            | !-- ccc
            |""".stripMargin
      }

      it("... or not") {

        // TODO: this is wrong! second line is not flushed
        tree2.showHierarchy.treeString shouldBe
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

  describe(Hierarchy.Indent2Minimal.productPrefix) {
    implicit lazy val format = Hierarchy.Indent2Minimal

    describe("treeString") {
      it("supports nodes each with 1 line str") {

        tree1.showHierarchy.treeString shouldBe
          """
            |aaa
            | ‣ bbb
            | :  ‣ ddd
            | ‣ ccc
            |""".stripMargin

      }

      it("... or not") {

        // TODO: this is wrong! second line is not flushed
        tree2.showHierarchy.treeString shouldBe
          """
            |aaa
            |%%%%%
            | ‣ bbb
            | : %%%%%
            | :  ‣ ddd
            | :    %%%%%
            | ‣ ccc
            |   %%%%%
            |""".stripMargin
      }
    }
  }
}
