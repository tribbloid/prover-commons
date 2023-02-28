package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.TreeFixture

class HierarchySpec extends TreeFixture {

  describe(Hierarchy.Indent2.productPrefix) {
    lazy val format = Hierarchy.Indent2

    describe("treeString") {

      describe("finite tree") {
        it("each node has 1 line of text") {

          tn1.tree.diagram_hierarchy.treeString shouldBe
            """
              |+ aaa
              |!-+ bbb
              |: !-- ddd
              |!-- ccc
              |""".stripMargin
        }

        it("... with arrow texts") {

          tn1.treeWithArrowTexts.diagram_hierarchy.treeString shouldBe
            """
              |+ aaa
              |!-: [ aaa |> bbb ]
              |: + bbb
              |: !-: [ bbb |> ddd ]
              |:   - ddd
              |!-: [ aaa |> ccc ]
              |  - ccc
              |""".stripMargin
        }

        it("each node has multiple lines of text") {

          tn2.tree.diagram_hierarchy.treeString shouldBe
            """
              |+ aaa
              |: %%%%%
              |!-+ bbb
              |: : %%%%%
              |: !-- ddd
              |:     %%%%%
              |!-- ccc
              |    %%%%%
              |""".stripMargin
        }

        it(".... with arrow texts") {

          tn2.treeWithArrowTexts.diagram_hierarchy.treeString shouldBe
            """
              |+ aaa
              |: %%%%%
              |!-: ┏ aaa          ┓
              |: : ┃ %%%%% |> bbb ┃
              |: : ┗ %%%%%        ┛
              |: + bbb
              |: : %%%%%
              |: !-: ┏ bbb          ┓
              |:   : ┃ %%%%% |> ddd ┃
              |:   : ┗ %%%%%        ┛
              |:   - ddd
              |:     %%%%%
              |!-: ┏ aaa          ┓
              |  : ┃ %%%%% |> ccc ┃
              |  : ┗ %%%%%        ┛
              |  - ccc
              |    %%%%%
              |""".stripMargin
        }
      }

      it("infinite tree") {
        treeInf.tree.diagram_hierarchy.treeString shouldBe
          """
            |+ abcdefgh
            |!-+ abcdefg
            |: !-+ abcdef
            |: : !-+ abcde
            |: : : !-+ abcd
            |: : : : !-- abc
            |: : : : !-- bcd
            |: : : !-+ bcde
            |: : :   !-- bcd
            |: : :   !-- cde
            |: : !-+ bcdef
            |: :   !-+ bcde
            |: :   : !-- bcd
            |: :   : !-- cde
            |: :   !-+ cdef
            |: :     !-- cde
            |: :     !-- def
            |: !-+ bcdefg
            |:   !-+ bcdef
            |:   : !-+ bcde
            |:   : : !-- bcd
            |:   : : !-- cde
            |:   : !-+ cdef
            |:   :   !-- cde
            |:   :   !-- def
            |:   !-+ cdefg
            |:     !-+ cdef
            |:     : !-- cde
            |:     : !-- def
            |:     !-+ defg
            |:       !-- def
            |:       !-- efg
            |!-+ bcdefgh
            |  !-+ bcdefg
            |  : !-+ bcdef
            |  : : !-+ bcde
            |  : : : !-- bcd
            |  : : : !-- cde
            |  : : !-+ cdef
            |  : :   !-- cde
            |  : :   !-- def
            |  : !-+ cdefg
            |  :   !-+ cdef
            |  :   : !-- cde
            |  :   : !-- def
            |  :   !-+ defg
            |  :     !-- def
            |  :     !-- efg
            |  !-+ cdefgh
            |    !-+ cdefg
            |    : !-+ cdef
            |    : : !-- cde
            |    : : !-- def
            |    : !-+ defg
            |    :   !-- def
            |    :   !-- efg
            |    !-+ defgh
            |      !-+ defg
            |      : !-- def
            |      : !-- efg
            |      !-+ efgh
            |        !-- efg
            |        !-- fgh
            |""".stripMargin
      }
    }
  }

  describe(Hierarchy.Indent2Minimal.productPrefix) {
    implicit lazy val format = Hierarchy.Indent2Minimal

    describe("treeString") {
      it("supports nodes each with 1 line str") {

        tn1.tree.diagram_hierarchy.treeString shouldBe
          """
            |aaa
            | ‣ bbb
            | :  ‣ ddd
            | ‣ ccc
            |""".stripMargin

      }

      it("... or not") {

        // TODO: this is wrong! second line is not flushed
        tn2.tree.diagram_hierarchy.treeString shouldBe
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
