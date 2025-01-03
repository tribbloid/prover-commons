package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.testlib.BaseSpec

class VisualisationSpec extends BaseSpec {

  for (
    viz <- Seq(
      Hierarchy.Default,
      LinkedHierarchy.Default,
      Flow.Default
    )
  ) {

    it(s"${viz.getClass} plot empty") {

      val tt: viz.applicableToType.type = viz.applicableToType

//      implicitly[tt.type <:< Local.GraphType[_, _]]

      val empty: viz.MaxGraph[Int] = tt.empty[Int]

      val s = viz.show(empty).toString

      assert(s == "")
    }
  }
}
