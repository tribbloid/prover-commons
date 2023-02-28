package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture

class LinkedHierarchySpec extends GraphFixture {

  describe(Hierarchy.Indent2.productPrefix) {
    lazy val format = Hierarchy.Indent2

    describe("treeString") {
      it("cyclic graph") {

        cyclic.graph.diagram_linkedHierarchy.treeString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |!-+ bbb
            |  !-+ ccc
            |  : !-- aaa ............................................................................. [0]
            |  !-- ddd
            |""".stripMargin

        cyclic.graphWithArrowText.diagram_linkedHierarchy.treeString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |!-: [ aaa |> bbb ]
            |  + bbb
            |  !-: [ bbb |> ccc ]
            |  : + ccc
            |  : !-: [ ccc |> aaa ]
            |  :   - aaa ............................................................................. [0]
            |  !-: [ bbb |> ddd ]
            |    - ddd
            |""".stripMargin
      }

      it(" ... with multiple lines") {

        cyclic2.graph.diagram_linkedHierarchy.treeString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |: %%%%
            |!-+ bbb
            |  : %%%%
            |  !-+ ccc
            |  : : %%%%
            |  : !-- aaa ............................................................................. [0]
            |  :     %%%%
            |  !-- ddd
            |      %%%%
            |""".stripMargin

        cyclic2.graphWithArrowText.diagram_linkedHierarchy.treeString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |: %%%%
            |!-: ┏ aaa         ┓
            |  : ┃ %%%% |> bbb ┃
            |  : ┗ %%%%        ┛
            |  + bbb
            |  : %%%%
            |  !-: ┏ bbb         ┓
            |  : : ┃ %%%% |> ccc ┃
            |  : : ┗ %%%%        ┛
            |  : + ccc
            |  : : %%%%
            |  : !-: ┏ ccc         ┓
            |  :   : ┃ %%%% |> aaa ┃
            |  :   : ┗ %%%%        ┛
            |  :   - aaa ............................................................................. [0]
            |  :     %%%%
            |  !-: ┏ bbb         ┓
            |    : ┃ %%%% |> ddd ┃
            |    : ┗ %%%%        ┛
            |    - ddd
            |      %%%%
            |""".stripMargin
      }

      it("diamond semilattice") {

        // TODO: this is wrong! second line is not flushed
        diamond.graph.diagram_linkedHierarchy.treeString shouldBe
          """
            |+ aaa
            |!-+ bbb
            |: !-+ ddd ............................................................................. [0]
            |:   !-- eee
            |!-+ ccc
            |  !-- ddd ............................................................................. [0]
            |""".stripMargin
      }
    }
  }
}
