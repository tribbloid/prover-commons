package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture

class HierarchyWithRefSpec extends GraphFixture {

  describe(Hierarchy.Indent2.productPrefix) {
    lazy val format = Hierarchy.Indent2

    describe("treeString") {
      it("cyclic graph") {

        cyclic.showHierarchyWithRef.treeString shouldBe
          """
            |-+ aaa ...... [1]
            | !-+ bbb
            |   !-+ ccc
            |   : !-- aaa ...... [1]
            |   !-- ddd
            |""".stripMargin
      }

      it("diamond semilattice") {

        // TODO: this is wrong! second line is not flushed
        diamond.showHierarchyWithRef.treeString shouldBe
          """
            |-+ aaa
            | !-+ bbb
            | : !-+ ddd ...... [1]
            | :   !-- eee
            | !-+ ccc
            |   !-- ddd ...... [1]
            |""".stripMargin
      }
    }
  }
}
