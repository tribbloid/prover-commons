package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.testlib.BaseSpec

object PolySpec {}

class PolySpec extends BaseSpec {

  describe("Poly") {

    import ai.acyclic.prover.commons.jit.fixture.Polys.*

    describe("case definition") {

      ignore("single-abstract method") {
        // current compiler is janky
      }
    }

    describe("summoning cases") {

      it("with input & output types") {

        _poly.at[Int].to[Int].summon
      }

      it("with input type only") {
        val v = _poly.at[Int].summon
        val v2 = _poly.at[Int].summon

//        implicitly[v.type <:< (_poly.Case[Int, Int])]
        assert(v == v2)

        val r = v.apply(1)
        r: Int
      }
    }

  }

}
