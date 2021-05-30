package org.shapesafe.graph.commons.util.reflect

import org.shapesafe.graph.commons.testlib.BaseSpec
import org.shapesafe.graph.commons.util.viz.TypeViz

import scala.language.reflectiveCalls

object TypeViewsSpec2 {

  val a = 3

  val b = new Object {

    val c = 3
  }
}
