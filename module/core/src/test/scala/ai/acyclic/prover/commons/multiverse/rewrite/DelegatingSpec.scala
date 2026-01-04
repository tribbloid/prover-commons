package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.testlib.BaseSpec

class DelegatingSpec extends BaseSpec {

  import DelegatingSpec.*

  describe("Delegating") {

    it("implicitly to the underlying type") {

      val d = D1(1)
      val i: Int = d

      assert(i == 1)
      assert((d: Int) == 1)
    }

    describe("recursively to the underlying type") {

      it("level 2") {

        val d = D2(D1(2))

        val i3: Int = d
        assert(i3 == 2)
        assert(d + 3 == 5)
      }

      it("level 3") {

        val d = D3(D2(D1(3)))

        val i3: Int = d
        assert(i3 == 3)
        assert(d + 7 == 10)
      }

      it("level 4") {

        val d = D4(D3(D2(D1(4))))

        val i4: Int = d
        assert(i4 == 4)
        assert(d + 6 == 10)
      }

      it("level 5") {

        val d = D5(D4(D3(D2(D1(5)))))

        val i5: Int = d
        assert(i5 == 5)
        assert(d + 5 == 10)
      }
    }
  }
}

object DelegatingSpec {

  case class D1(unbox: Int) extends Delegating[Int]

  case class D2(unbox: Delegating[Int]) extends Delegating[Delegating[Int]]

  case class D3(unbox: Delegating[Delegating[Int]]) extends Delegating[Delegating[Delegating[Int]]]

  case class D4(unbox: Delegating[Delegating[Delegating[Int]]])
    extends Delegating[Delegating[Delegating[Delegating[Int]]]]

  case class D5(unbox: Delegating[Delegating[Delegating[Delegating[Int]]]])
    extends Delegating[Delegating[Delegating[Delegating[Delegating[Int]]]]]
}
