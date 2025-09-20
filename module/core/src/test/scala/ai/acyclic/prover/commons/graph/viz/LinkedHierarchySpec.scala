package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.testlib.BaseSpec

class LinkedHierarchySpec extends BaseSpec {

  import GraphFixture.*

  describe(Hierarchy.Indent2.productPrefix) {
//    lazy val format = Hierarchy.Indent2

    describe("treeString") {
      it("cyclic graph") {

        Local(cyclic*).text_linkedHierarchy().toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |!-+ bbb
            |  !-+ ccc
            |  : !-- aaa ... (see [0])
            |  !-- ddd
            |""".stripMargin

        Local(cyclic.withArrows*).text_linkedHierarchy().toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |!-⟦ aaa |> bbb ⟧+ bbb
            |                !-⟦ bbb |> ccc ⟧+ ccc
            |                :               !-⟦ ccc |> aaa ⟧- aaa ... (see [0])
            |                !-⟦ bbb |> ddd ⟧- ddd
            |""".stripMargin
      }

      it(" ... with multiple lines") {

        Local(cyclic2*).text_linkedHierarchy().toString shouldBe
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

        Local(cyclic2.withArrows*).text_linkedHierarchy().toString shouldBe
          """
            |+ aaa ............................................................................. [0]
            |: %%%%
            |!-┏ aaa         ┓+ bbb
            |  ┃ %%%% |> bbb ┃: %%%%
            |  ┗ %%%%        ┛!-┏ bbb         ┓+ ccc
            |                 : ┃ %%%% |> ccc ┃: %%%%
            |                 : ┗ %%%%        ┛!-┏ ccc         ┓- aaa ... (see [0])
            |                 :                  ┃ %%%% |> aaa ┃  %%%%
            |                 :                  ┗ %%%%        ┛
            |                 !-┏ bbb         ┓- ddd
            |                   ┃ %%%% |> ddd ┃  %%%%
            |                   ┗ %%%%        ┛
            |""".stripMargin
      }

      it("diamond semilattice") {

        Local(diamond*).text_linkedHierarchy().toString shouldBe
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
