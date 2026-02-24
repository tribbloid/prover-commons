package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.jit.hom.Hom.Const
import ai.acyclic.prover.commons.testlib.BaseSpec
import Args.{><:, T0}

object HasArgsSpec {}

class HasArgsSpec extends BaseSpec {

  describe("Args.of constructors should") {

    describe("construct via applyProduct") {

      it("unary") {
        val original: 1 ><: T0 = Args.><:(Const.Provided(1), T0)

        val viaOf = Args.of(Const.Provided(1))

        assert(viaOf == original)
      }
    }

    describe("FromProductOrValue should") {

      it("map Unit to Eye") {
        val result = Args.FromProductOrValue(())

        val _: T0 = result
        val _: Args.Prod = result

        assert(result == T0)
      }

      it("map ConstantFn value to singleton Prod") {
        val value: Args.Element[Int] = Const.Provided(1)
        val result = Args.FromProductOrValue(value)

        val _: Int ><: T0 = result
        val _: Args.Prod = result

        val (head, tail) = Args.deCons(result)
        assert(head.compute == 1) E
          assert(tail == T0)
      }
    }

  }
}
