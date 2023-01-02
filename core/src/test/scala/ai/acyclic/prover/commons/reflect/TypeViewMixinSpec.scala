package ai.acyclic.prover.commons.reflect

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.viz.TypeViz

class TypeViewMixinSpec extends BaseSpec {

  describe("getOnlyInstance on") {

    it("object") {

      val v = TypeViz[TypeViewMixinSpec.type].typeView
      assert(v.onlyInstance == TypeViewMixinSpec)
    }

    it("constant") {

      val v = TypeViz[3].typeView
      assert(v.onlyInstance == 3)
    }

    it("unique value") {

      val v = TypeViz[TypeViewMixinSpec.a.type].typeView
      assert(v.onlyInstance == 3)
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

        assert(v.onlyInstance == 3)
      }
    }

  }
}

object TypeViewMixinSpec {

  val a = 3

  val b = new Object {

    val c = 3
  }
}
