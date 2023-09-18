package ai.acyclic.prover.commons.notebook

import ai.acyclic.prover.commons.debug.print_@
import ai.acyclic.prover.commons.testlib.BaseSpec

object SplainPluginVersions extends ForwardPlot {

  val scalaV = Forward("What's your scala version?")

  val old = Forward("splain 0.5.x")
    .from(scalaV, "2.12")
    .from(scalaV, "2.13.0 ... 2.13.5")

  val features = Forward("Do you want to use the latest features?")
    .from(scalaV, "2.13.6 ... latest")

  val plugin = Forward("splain 1.x")
    .from(features, "yes")

  val buildIn = Forward("Scala compiler built-in")
    .from(features, "no")
}

class SplainPluginVersions extends BaseSpec {

  import SplainPluginVersions._

  it("plot") {

    val g = G(Seq(scalaV))

    print_@(g.diagram_flow.toString)

    print_@(g.diagram_linkedHierarchy.toString)
  }
}
