package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.testlib.BaseSpec

class FromTupleMixinSpec extends BaseSpec {

  import ai.acyclic.prover.commons.finset.Tuples
  import Tuples.*

  describe("FromFlatTuple") {

    it("should convert Unit to Empty") {
      val res = FromFlatTuple(())
      assert(res == Empty)
    }

    it("should allow invoking unitCase explicitly") {
      val res = FromFlatTuple.unitCase(())
      assert(res == Tuples._0)
    }

    it("should convert Tuple1 to Empty >< A") {
      val res = FromFlatTuple(Tuple1(1))
      assert(res == Empty >< 1)
    }

    it("should convert (A, B) to Empty >< A >< B") {
      val res = FromFlatTuple((1, "a"))
      assert(res == Empty >< "a" >< 1)
    }

    it("should convert (A, B, C) to Empty >< A >< B >< C") {
      val res = FromFlatTuple((1, "a", true))
      assert(res == Empty >< true >< "a" >< 1)
    }

    it("should fail compilation for non-tuple types") {
      assertDoesNotCompile("FromFlatTuple(1)")
    }
  }

  describe("FromFlat") {

    it("should convert Unit to Empty") {
      val res = FromFlat(())
      assert(res == Empty)
    }

    it("should convert Tuple1 to Empty >< A") {
      val res = FromFlat(Tuple1(1))
      assert(res == Empty >< 1)
    }

    it("should convert (A, B) to Empty >< A >< B") {
      val res = FromFlat((1, "a"))
      assert(res == Empty >< "a" >< 1)
    }

    it("should convert (A, B, C) to Empty >< A >< B >< C") {
      val res = FromFlat((1, "a", true))
      assert(res == Empty >< true >< "a" >< 1)
    }

    it("should convert atom to Empty >< A") {
      val res = FromFlat(1)
      assert(res == Empty >< 1)
    }

    it("should convert atom (String) to Empty >< A") {
      val res = FromFlat("abc")
      assert(res == Empty >< "abc")
    }
  }
}
