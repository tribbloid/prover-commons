package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.testlib.BaseSpec

object PolySpec {}

class PolySpec extends BaseSpec {

  describe("Poly") {

    import ai.acyclic.prover.commons.function.fixture.Polys.*

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

        implicitly[v.type <:< (_poly.Impl[Int, Int])]
        assert(v == v2)

        val r = v.apply(1)
        r: Int
      }
    }

    describe("lemma") {

      def __sanity[I, O](): Unit = {
        {
          def useIO(l: _poly.Impl[Int, String]) = {
            l.apply(1)
          }

          def useI(l: _poly.Impl.At[Int]) = {
            l.apply(1)
          }
        }

        {
          def useIO(l: _poly.Impl[I, O]) = {
            val v: I = ???
            l.apply(v)
          }

          def useI(l: _poly.Impl.At[I]) = {
            val v: I = ???
            l.apply(v)
          }
        }
      }

    }
  }

}
