package org.shapesafe.graph.commons.util.reflect

import org.shapesafe.graph.commons.testlib.BaseSpec
import org.shapesafe.graph.commons.util.viz.TypeViz

class TypeViewsSpec extends BaseSpec {

  describe("getOnlyInstance on") {

    it("object") {

      val v = TypeViz[TypeViewsSpec.type].typeView
      assert(v.getOnlyInstance == TypeViewsSpec)
    }

    it("constant") {

      val v = TypeViz[3].typeView
      assert(v.getOnlyInstance == 3)
    }

    it("unique value") {

      val v = TypeViz[TypeViewsSpec.a.type].typeView
      assert(v.getOnlyInstance == 3)
    }

//    it(" ... more complex") { TODO: compiler error
//
//      val v = TypeViz.narrow(TypeViewsSpec.b.c).typeView
//      assert(v.getOnlyInstance == 3)
//    }

    it("local value") {

      val a = 3

      val v = TypeViz[a.type].typeView
      intercept[UnsupportedOperationException] {

        assert(v.getOnlyInstance == 3)
      }
    }

  }
}

object TypeViewsSpec {

  val a = 3

  val b = new Object {

    val c = 3
  }
}
