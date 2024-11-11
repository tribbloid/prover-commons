package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.testlib.BaseSpec

class LinkedHierarchySpec extends BaseSpec {

  import GraphFixture.*

  describe(Hierarchy.Indent2.productPrefix) {
//    lazy val format = Hierarchy.Indent2

    describe("treeString") {
      it("cyclic graph") {

        cyclic.make.text_linkedHierarchy.toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |!-+ bbb
            |  !-+ ccc
            |  : !-- aaa ... (see [0])
            |  !-- ddd
            |""".stripMargin

        cyclic.withArrows.make.text_linkedHierarchy.toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |!-: ( aaa |> bbb )
            |  + bbb
            |  !-: ( bbb |> ccc )
            |  : + ccc
            |  : !-: ( ccc |> aaa )
            |  :   - aaa ... (see [0])
            |  !-: ( bbb |> ddd )
            |    - ddd
            |""".stripMargin
      }

      it(" ... with multiple lines") {

        cyclic2.make.text_linkedHierarchy.toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |: %%%%
            |!-+ bbb
            |  : %%%%
            |  !-+ ccc
            |  : : %%%%
            |  : !-- aaa ... (see [0])
            |  :     %%%%
            |  !-- ddd
            |      %%%%
            |""".stripMargin

        cyclic2.withArrows.make.text_linkedHierarchy.toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |: %%%%
            |!-: ⎛ aaa         ⎞
            |  : ⎢ %%%% |> bbb ⎟
            |  : ⎝ %%%%        ⎠
            |  + bbb
            |  : %%%%
            |  !-: ⎛ bbb         ⎞
            |  : : ⎢ %%%% |> ccc ⎟
            |  : : ⎝ %%%%        ⎠
            |  : + ccc
            |  : : %%%%
            |  : !-: ⎛ ccc         ⎞
            |  :   : ⎢ %%%% |> aaa ⎟
            |  :   : ⎝ %%%%        ⎠
            |  :   - aaa ... (see [0])
            |  :     %%%%
            |  !-: ⎛ bbb         ⎞
            |    : ⎢ %%%% |> ddd ⎟
            |    : ⎝ %%%%        ⎠
            |    - ddd
            |      %%%%
            |""".stripMargin
      }

      it("diamond semilattice") {

        diamond.make.text_linkedHierarchy.toString shouldBe
          """
            |+ aaa
            |!-+ bbb
            |: !-+ ddd ............................................................................. [0]
            |:   !-- eee
            |!-+ ccc
            |  !-- ddd ... (see [0])
            |""".stripMargin
      }
    }
  }
}
