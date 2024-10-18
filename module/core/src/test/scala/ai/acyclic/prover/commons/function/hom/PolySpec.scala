package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.testlib.BaseSpec

object PolySpec {}

class PolySpec extends BaseSpec {

  describe("Mono") {}

  describe("Poly") {

    import ai.acyclic.prover.commons.function.fixture.Polys._

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

        implicitly[v.type <:< (_poly.=>>[Int, Int])]
        assert(v == v2)

        val r = v.apply(1)
        r: Int
      }
    }
  }

}
