package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.jit.hom.Hom.Const
import ai.acyclic.prover.commons.testlib.BaseSpec
import Args.{><:, T0}

object HasArgsSpec {}

class HasArgsSpec extends BaseSpec {

  describe("Args constructors") {

    it("of/ofNarrow should preserve exact Prod via applyProduct") {
      val original: 1 ><: T0 = Args.><:(Const.Provided[1](1), T0)

      val viaOf = Args.of(Const.Provided(1), Const.Provided(2))

      val _: 1 ><: T0 = viaOf
      val _: 1 ><: T0 = viaOfNarrow
      val _: Args.Prod = viaOf
      val _: Args.Prod = viaOfNarrow

      assert(viaOf == original)
    }

    it("FromProductOrValue should map Unit to Eye") {
      val result = Args.FromProductOrValue(())

      val _: T0 = result
      val _: Args.Prod = result

      assert(result == T0)
    }

    it("FromProductOrValue should map ConstantFn value to singleton Prod") {
      val value: Args.Element[Int] = Const.Provided(1)
      val result = Args.FromProductOrValue(value)

      val _: Int ><: T0 = result
      val _: Args.Prod = result

      val (head, tail) = Args.deCons(result)
      assert(head.compute == 1)
      assert(tail == T0)
    }

    it("FromProductOrValue should treat tuple payload as one value for Args") {
      val value: Args.Element[(Int, String)] = Const.Provided((1, "a"))
      val result = Args.FromProductOrValue(Const.Provided(1), Const.Provided("a"))

      val _: (Int, String) ><: T0 = result
      val _: Args.Prod = result

      val (head, tail) = Args.deCons(result)
      assert(head.compute == (1, "a"))
      assert(tail == T0)
    }
  }
}
