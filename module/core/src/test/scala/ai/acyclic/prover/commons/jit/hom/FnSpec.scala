package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.jit.fixture.*
import ai.acyclic.prover.commons.jit.Hom.{Const, Fn}
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

object FnSpec {}

class FnSpec extends BaseSpec {

  import Circuits.*

  describe("define by") {

    it("inheriting from Impl") {

      case object cc extends Fn.Impl[Int ><: T0, String] {
        override lazy val noInput: Args.Schema[In] = Args.Schema.Eye.cons[Int]

        def apply(v: Int ><: T0): String = "" + v.head.compute
      }
      assert(
        (cc.apply(
          Const.Provided(1) ><: Args.eye
        ): String) == "1"
      )
    }

    it("implicit cast") {

      val cc: Fn[Int ><: T0, String] = { (v: Int) =>
        "" + v
      }
      assert(cc.getClass == classOf[Fn.Blackbox[?, ?]])
      assert(
        (cc.apply(
          Const.Provided(1) ><: Args.eye
        ): String) == "1"
      )

//      assert(useCircuit { _ =>
//        "1"
//      } == "1") // TODO: only works in Scala 3
    }
  }

  describe("copy without SrcPosition change") {
    it("should preserve equality") {

      val cc: Fn[Int ><: T0, String] = { (v: Int) =>
        "" + v
      }

      val cc1 = cc match {
        case bb: Fn.Blackbox[Int @unchecked, String @unchecked] => bb
        case other => fail(s"Expected Fn.Blackbox but got ${other.getClass}")
      }

      val cc2 = cc1.copy()(fn = { _: Int => "" })

      assert(cc == cc2)
    }
  }

  it("explain") {

    fn0.explain.nodeText.shouldBe(
      "Blackbox(fn0 <at Circuits.scala:8>)"
    )

    fn0.toString.shouldBe(
      fn0.explain.nodeText
    )

    fn0.explain
      .text_hierarchy()
      .shouldBe(
        s"- ${fn0.toString}"
      )
  }

  describe("chain") {

    import ai.acyclic.prover.commons.jit.fixture.{ChainOther, ChainSelf}

    describe("self") {

      ChainSelf.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.simplify
            normal.explain
              .text_hierarchy()
              .shouldBe(
                s
              )

            val in = Const.Provided(1) ><: Args.eye
            val r1 = fn.apply(in)
            assert(r1 == 3)
          }
      }

    }

    describe("other") {

      ChainOther.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.simplify
            normal.explain
              .text_hierarchy()
              .shouldBe(
                s
              )

            val in = Const.Provided(1) ><: Args.eye
            val r1 = fn.apply(in)
            assert(r1 == "2b")
          }
      }
    }

//    it("other-tracing-structure") {
//
//      val h3 = ChainOther.s3.explain.text_hierarchy()
//      val h4 = ChainOther.s4.explain.text_hierarchy()
//
//      println(h3)
//      println("===")
//      println(h4)
//    }

    describe("twice") {

      ChainTwice.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.simplify

            val sLeft =
              normal.explain
                .text_hierarchy()

            sLeft.shouldBe(s)

            val in = Const.Provided(1) ><: Args.eye
            val r1 = fn.apply(in)
            assert(r1 == "10b")
          }
      }
    }

  }

  describe("pointwise") {

    ZippedLike.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it(i.toString) {

          val normal = fn.simplify
          normal.explain
            .text_hierarchy()
            .shouldBe(
              s
            )

          val combinedIn = Const.Provided(1) ><: (Const.Provided(2L) ><: Args.eye)

          val fnTraced = ai.acyclic.prover.commons.jit.cps.Continuation.tracingToFunction(fn)
          val r1 = fnTraced(combinedIn)
          val expected =
            if (i == 0) {
              (List(1L, 2L, 3L), List(2.0, 2.1, 2.2))
            } else {
              List(3.0, 4.1, 5.2)
            }
          assert(r1 == expected)
        }
    }

    describe("direct construction") {

      it("with single-arg tail") {

        val head: Fn[Int ><: T0, String] = { (v: Int) => "h" + v }
        val tail: Fn[Long ><: T0, Double] = { (v: Long) => v * 0.5 }

        val pw = Fn.zip(head, tail)

        val in = Const.Provided(10) ><: (Const.Provided(4L) ><: Args.eye)

        val result = pw.apply(in)
        assert(result == ("h10", 2.0))
      }

      it("with multi-arg tail") {

        val head: Fn[Int ><: T0, String] = { (v: Int) => "v=" + v }

        val tail: Fn[String ><: Long ><: T0, (String, Long)] =
          Fn.zip(
            (s: String) => s.toUpperCase,
            (l: Long) => l + 100L
          )

        val pw = Fn.zip(head, tail)

        val in = Const.Provided(42) ><: (Const.Provided("abc") ><: (Const.Provided(7L) ><: Args.eye))

        val result = pw.apply(in)
        assert(result == ("v=42", ("ABC", 107L)))
      }

      it("tree structure") {

        val head: Fn[Int ><: T0, String] = { (v: Int) => "h" + v }
        val tail: Fn[Long ><: T0, Double] = { (v: Long) => v * 0.5 }

        val pw = Fn.zip(head, tail)

        val tree = pw.explain.text_hierarchy()
        assert(tree.contains("Zipped"))
        assert(tree.contains("Blackbox"))
      }

      it("simplify preserves function") {

        val head: Fn[Int ><: T0, String] = { (v: Int) => "s" + v }
        val tail: Fn[Long ><: T0, Double] = { (v: Long) => v.toDouble }

        val pw = Fn.zip(head, tail)
        val simplified = pw.simplify

        val in = Const.Provided(5) ><: (Const.Provided(3L) ><: Args.eye)

        val result = simplified.apply(in)
        assert(result == ("s5", 3.0))
      }
    }
  }

  describe("fork") {

    ForkLike.pairs.zipWithIndex.foreach {

      case ((fn, _), i) =>
        it(i.toString) {

          val normal = fn.simplify
          val tree = normal.explain.text_hierarchy()
          assert(tree.contains("Zipped"))

          val combinedIn = Const.Provided(1) ><: (Const.Provided(2L) ><: Args.eye)

          val fnTraced = ai.acyclic.prover.commons.jit.cps.Continuation.tracingToFunction(fn)
          val result = fnTraced(combinedIn)

          if (i == 0) {
            assert(result == (List(1L, 2L, 3L), List(2.0, 2.1, 2.2)))
          } else {
            assert(result == List(3.0, 4.1, 5.2))
          }
        }
    }

    it("simplify preserves behavior") {

      val combinedIn = Const.Provided(1) ><: (Const.Provided(2L) ><: Args.eye)

      ForkLike.pairs.foreach {
        case (fn, _) =>
          val fnTraced = ai.acyclic.prover.commons.jit.cps.Continuation.tracingToFunction(fn)
          val original = fnTraced(combinedIn)
          val simplifiedFn = fn.simplify
          val simplified = simplifiedFn(combinedIn)
          assert(original == simplified)
      }
    }
  }

  describe("flatten") {

    it("basic usage") {
      val baseFn: Fn[Int ><: T0, Int] = { (v: Int) => v * 2 }
      val coerceFn: Int => Fn[Int ><: T0, String] = { (t: Int) =>
        { (v: Int) => s"v=$v, t=$t" }
      }

      val flatten = Fn.Flatten(baseFn, coerceFn)

      val arg = Const.Provided(3) ><: Args.eye
      val result = flatten.apply(arg)
      assert(result == "v=3, t=6")
    }

    it("simplify preserves behavior") {
      val baseFn: Fn[Int ><: T0, Int] = { (v: Int) => v * 2 }
      val coerceFn: Int => Fn[Int ><: T0, String] = { (t: Int) =>
        { (v: Int) => s"v=$v, t=$t" }
      }

      val flatten = Fn.Flatten(baseFn, coerceFn)
      val simplified = flatten.simplify

      val arg = Const.Provided(4) ><: Args.eye
      assert(flatten.apply(arg) == simplified.apply(arg))
    }
  }

  describe("higher-order") {

    import ai.acyclic.prover.commons.jit.fixture.HigherOrder1

    HigherOrder1.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it("1-" + i.toString) {

          val normal = fn.simplify
          normal.explain
            .text_hierarchy()
            .shouldBe(
              s
            )
        }
    }

    HigherOrder2.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it("2-" + i.toString) {

          val normal = fn.simplify
          normal.explain
            .text_hierarchy()
            .shouldBe(
              s
            )

          //          val r1 = fn.apply(1 -> 2L)
        }
    }
  }

}
