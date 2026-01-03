package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.testlib.BaseSpec

class DelegatingSpec extends BaseSpec {

  import DelegatingSpec.*
  import Delegating.*
  import Delegating.Conversion

  describe("Delegating") {

    it("can unbox implicitly to the underlying type") {

      val d = D1(1)
      val i: Int = d

      assert(i == 1)
      assert(d.as[Int] == 1)
    }

    describe("can unbox recursively") {

      it("explicitly") {
        val d = D2(D1(2))
        val i1: Int = d.as[Int]
        val i2: Int = unboxImplicitly[Delegating[Int], Int](d)

        assert(i1 == 2)
        assert(i2 == 2)
      }

      it("implicitly") {

        val d = D2(D1(2))

        val i3: Int = d
        assert(i3 == 2)
        assert(d + 3 == 5)
      }
    }

    it("can unbox and then apply a custom Conversion") {

      implicit val int2String: Conversion[Int, String] = new Conversion[Int, String] {
        override def apply(v: Int): String = s"v=$v"
      }

      val d = D2(D1(3))
      val s1: String = d.as[String]
      val s2: String = unboxImplicitly[Delegating[Int], String](d)

      assert(s1 == "v=3")
      assert(s2 == "v=3")
    }

    it("does not compile if no Conversion exists") {

      shouldNotCompile(
        """import ai.acyclic.prover.commons.Delegating
          
          case class D(unbox: Int) extends Delegating[Int]
          
          case class Foo(i: Int)
          
          val d: D = D(1)
          val foo: Foo = d
          """
      )
    }
  }
}

object DelegatingSpec {

  case class D1(unbox: Int) extends Delegating[Int]

  case class D2(unbox: Delegating[Int]) extends Delegating[Delegating[Int]]
}
