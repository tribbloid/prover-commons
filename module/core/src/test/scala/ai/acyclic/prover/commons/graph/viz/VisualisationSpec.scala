package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.testlib.BaseSpec

class VisualisationSpec extends BaseSpec {

  for (
    viz: Visualisation.OfType <- Seq(
      Hierarchy.Default,
      LinkedHierarchy.Default,
      Flow.Default
    )
  ) {

    it(s"${viz.getClass} plot empty") {

      val tt: viz.applicableToType.type = viz.applicableToType

//      implicitly[tt.type <:< Local.GraphType[_, _]]

      val empty: viz.Graph_/\[Int] = tt.empty[Int]

      val s = viz.visualise(empty).toString

      assert(s == "")
    }
  }
}
