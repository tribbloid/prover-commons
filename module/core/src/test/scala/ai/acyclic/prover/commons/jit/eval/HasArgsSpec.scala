package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.testlib.BaseSpec
import Args.{><:, T0}

object HasArgsSpec {}

class HasArgsSpec extends BaseSpec {

  describe("Args.of constructors should") {

    describe("construct via applyProduct") {

      it("unary") {
        val original: 1 ><: T0 = Args.><:(Const.Provided(1), T0)

        val viaOf = Args.of(Const.Provided(1))

        val _: Int ><: T0 = viaOf

        assert(viaOf == original)
      }

      it("binary and keep order") {
        val original: Int ><: String ><: T0 = Args.><:(Const.Provided(1), Args.><:(Const.Provided("a"), T0))

        val viaOf = Args.of(Const.Provided(1), Const.Provided("a"))

        val _: Int ><: String ><: T0 = viaOf

        val (head, tail) = Args.deCons(viaOf)
        val (head2, tail2) = Args.deCons(tail)

        assert(viaOf == original)
        assert(head.compute == 1)
        assert(head2.compute == "a")
        assert(tail2 == T0)
      }

      it("ofNarrow should construct Prod") {
        val viaOfNarrow = Args.ofNarrow(Const.Provided[1](1), Const.Provided["a"]("a"))

        val _: Int ><: String ><: T0 = viaOfNarrow
        val _: Args = viaOfNarrow

        val (head, tail) = Args.deCons(viaOfNarrow)
        val (head2, _) = Args.deCons(tail)

        assert(head.compute == 1)
        assert(head2.compute == "a")
      }
    }

    describe("FromProductOrValue should") {

      it("map Unit to Eye") {
        val result = Args.FromProductOrValue(())

        val _: T0 = result
        val _: Args = result

        assert(result == T0)
      }

      it("map ConstantFn value to singleton Prod") {
        val value: Args.Element[Int] = Const.Provided(1)
        val result = Args.FromProductOrValue(value)

        val _: Int ><: T0 = result
        val _: Args = result

        val (head, tail) = Args.deCons(result)
        assert(head.compute == 1)
        assert(tail == T0)
      }

      it("keep value sequence aligned with runtime sequence") {
        val result = Args.of(
          Const.Provided(1),
          Const.Provided("a"),
          Const.Provided(true)
        )

        assert(result.runtimeSeq.map(_.compute) == Seq(1, "a", true))
        assert(result.valueSeq == Seq(1, "a", true))
      }

      it("wrap ConstantFn NotProvided without forcing evaluation") {
        val value: Args.Element[Int] = Const.NotProvided
        val result = Args.FromProductOrValue(value)

        val (head, tail) = Args.deCons(result)

        assert(head eq Const.NotProvided)
        assert(tail == T0)
        assertThrows[NoSuchElementException](head.compute)
      }
    }

  }
}
