//package ai.acyclic.prover.commons.graph.local.ops
//
//import ai.acyclic.prover.commons.graph.GraphFixture
//import ai.acyclic.prover.commons.graph.local.Local
//import ai.acyclic.prover.commons.testlib.BaseSpec
//
//class GraphBinarySpec extends BaseSpec {
//
//  import GraphFixture._
//
//  describe("Union") {
//    it("of graphs") {
//
//      val g1 = DuplicatedB.part1.graph
//      val g2 = DuplicatedB.part2.graph
//
//      val uu = GraphUnary.^(g1).&&(g2).Union().resolve
//
//      val max = uu.asInstanceOf[Local.Semilattice.Upper[GraphFixture.GV]].maxNodeOpt
//
//      uu.diagram_Hasse.treeString.shouldBe(
//      )
//    }
//  }
//}
