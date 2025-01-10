package ai.acyclic.prover.commons.meta

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.viz.TypeViz
import shapeless.Witness

class TypeViewMixinSpec extends BaseSpec {

  import TypeViewMixinSpec.*

  describe("getOnlyInstance on") {

    it("object") {

      val v = TypeViz.apply[TypeViewMixinSpec.type].typeOps
      assert(v.onlyInstance == TypeViewMixinSpec)
    }

    it("constant") {

      val v = TypeViz[3].typeOps
      assert(v.onlyInstance == 3)
    }

    it("unique value") {

      val v = TypeViz[TypeViewMixinSpec.singleton.type].typeOps
      assert(v.onlyInstance == 3)
    }

//    it(" ... more complex") { TODO: compiler error
//
//      val v = TypeViz.narrow(TypeViewsSpec.b.c).typeView
//      assert(v.getOnlyInstance == 3)
//    }

    it("local value") {

      val a = 3

      val v = TypeViz[a.type].typeOps
      intercept[UnsupportedOperationException] {

        assert(v.onlyInstance == 3)
      }
    }
  }

  describe("isBuiltIn") {

    it("scala built-in type") {

      val v = TypeViz[Product].typeOps
      assert(v.isBuiltIn)
    }

    describe("java built-in type") {

      it("1") {

        val v = TypeViz[String].typeOps
        assert(v.isBuiltIn)
      }

      it("2") {

        val v = TypeViz[Object].typeOps
        assert(v.isBuiltIn)
      }
    }

    it("mixin of built-in types") {

      val v = TypeViz[Serializable & Product].typeOps
      assert(!v.isBuiltIn)
    }

    it("singleton") {

      val v = TypeViz[singleton.type].typeOps
      assert(!v.isBuiltIn)
    }

    it("local") {

      val w = 3

      val v = TypeViz[w.type].typeOps
      assert(!v.isBuiltIn)
    }

    it("witness") {

      val v = TypeViz[singletonW.T].typeOps
      assert(!v.isBuiltIn)
    }
  }
}

object TypeViewMixinSpec {

  val singleton = 3

  val singletonW = Witness(3)
//  val b = new Object {
//
//    val c = 3
//  }
}
