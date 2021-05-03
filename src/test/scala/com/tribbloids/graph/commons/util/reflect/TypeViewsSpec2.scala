package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.viz.TypeViz

import scala.language.reflectiveCalls

object TypeViewsSpec2 {

  val a = 3

  val b = new Object {

    val c = 3
  }
}
