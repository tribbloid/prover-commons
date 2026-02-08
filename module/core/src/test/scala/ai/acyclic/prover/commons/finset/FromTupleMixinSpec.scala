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

    it("should convert Tuple1 to A ><: Empty") {
      val res = FromFlatTuple(Tuple1(1))
      assert(res == 1 ><: Empty)
    }

    it("should convert (A, B) to A ><: B ><: Empty") {
      val res = FromFlatTuple((1, "a"))
      assert(res == 1 ><: "a" ><: Empty)
    }

    it("should convert (A, B, C) to A ><: B ><: C ><: Empty") {
      val res = FromFlatTuple((1, "a", true))
      assert(res == 1 ><: "a" ><: true ><: Empty)
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

    it("should convert Tuple1 to A ><: Empty") {
      val res = FromFlat(Tuple1(1))
      assert(res == 1 ><: Empty)
    }

    it("should convert (A, B) to A ><: B ><: Empty") {
      val res = FromFlat((1, "a"))
      assert(res == 1 ><: "a" ><: Empty)
    }

    it("should convert (A, B, C) to A ><: B ><: C ><: Empty") {
      val res = FromFlat((1, "a", true))
      assert(res == 1 ><: "a" ><: true ><: Empty)
    }

    it("should convert atom to A ><: Empty") {
      val res = FromFlat(1)
      assert(res == 1 ><: Empty)
    }

    it("should convert atom (String) to A ><: Empty") {
      val res = FromFlat("abc")
      assert(res == "abc" ><: Empty)
    }
  }
}
