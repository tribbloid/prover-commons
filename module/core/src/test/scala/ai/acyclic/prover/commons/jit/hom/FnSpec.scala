package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.jit.fixture.*
import ai.acyclic.prover.commons.jit.Hom.{Const, Fn, Fn1, Fn2}
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.jit.eval.{Args, PartialEvalEnv}
import Args.{><:, T0}

object FnSpec {}

class FnSpec extends BaseSpec {

  private def fullyProvidedEnv[I <: Args](arg: I): () => PartialEvalEnv[I] = { () =>
    PartialEvalEnv(arg, failFast = false, onlyPure = false)
  }

  private def assertDirectEvalAndPartialEval[I <: Args, O](
      fn: Fn[I, O],
      arg: I,
      expected: O,
      expectConstant: Boolean
  ): Unit = {

    assert(fn.apply(arg) == expected)

    val partial = fn.partialEval(fullyProvidedEnv(arg))
    assert(partial.apply(arg) == expected)

    val partialConst = partial.asConstantOrNone
    if (expectConstant) {
      assert(partialConst.get.value == expected)
    } else {
      assert(partialConst.isEmpty)
    }
  }

  import Circuits.*

  describe("define by") {

    it("inheriting from Impl") {

      case object cc extends Fn.Impl1[Int, String] {

        def apply(v: Int ><: T0): String = "" + v.head.value
      }
      assert(
        (cc.apply(
          Const.Provided(1) ><: Args.eye
        ): String) == "1"
      )
    }

    it("implicit cast") {

      val cc: Fn1[Int, String] = { (v: Int) =>
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

  describe("Blackbox") {
    it("copy should preserve equality") {

      val cc: Fn1[Int, String] = { (v: Int) =>
        "" + v
      }

      val cc1 = cc match {
        case bb: Fn.Blackbox[?, ?] => bb
        case other                 => fail(s"Expected Fn.Blackbox but got ${other.getClass}")
      }

      val cc2 = cc1.copy()(fn = { _: Int => "" })
      assert(cc == cc2)
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

    it("partialEval") {
      Circuits.fn0 match {
        case _: Fn.Blackbox[?, ?] => succeed
        case other                => fail(s"Expected Fn.Blackbox but got ${other.getClass}")
      }

      assertDirectEvalAndPartialEval(
        Circuits.fn0,
        Const.Provided(1) ><: Args.eye,
        2,
        expectConstant = true
      )
    }
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

  describe("Mapped") {

    it("partialEval") {
      ChainSelf.s0 match {
        case _: Fn.Mapped[?, ?, ?] => succeed
        case other                 => fail(s"Expected Fn.Mapped but got ${other.getClass}")
      }

      assertDirectEvalAndPartialEval(
        ChainSelf.s0,
        Const.Provided(1) ><: Args.eye,
        3,
        expectConstant = true
      )
    }
  }

  describe("Identity") {

    it("embedded fixture node can be fully partially evaluated") {
      val identity = ChainSelf.s2.simplify match {
        case outer: (Fn.Mapped[Int ><: T0, Int, Int] @unchecked) =>
          outer.left match {
            case inner: (Fn.Mapped[Int ><: T0, Int, Int] @unchecked) =>
              inner.left match {
                case id: (Fn.Identity[Int] @unchecked) => id
                case other                             => fail(s"Expected Fn.Identity but got ${other.getClass}")
              }
            case other => fail(s"Expected nested Fn.Mapped but got ${other.getClass}")
          }
        case other => fail(s"Expected Fn.Mapped but got ${other.getClass}")
      }

      assertDirectEvalAndPartialEval(
        identity,
        Const.Provided(7) ><: Args.eye,
        7,
        expectConstant = true
      )
    }
  }

  describe("Zipped") {

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

        val head: Fn1[Int, String] = { (v: Int) => "h" + v }
        val tail: Fn1[Long, Double] = { (v: Long) => v * 0.5 }

        val pw = Fn.zip(head, tail)

        val in = Const.Provided(10) ><: (Const.Provided(4L) ><: Args.eye)

        val result = pw.apply(in)
        assert(result == ("h10", 2.0))
      }

      it("with multi-arg tail") {

        val head: Fn1[Int, String] = { (v: Int) => "v=" + v }

        val tail: Fn2[String, Long, (String, Long)] =
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

        val head: Fn1[Int, String] = { (v: Int) => "h" + v }
        val tail: Fn1[Long, Double] = { (v: Long) => v * 0.5 }

        val pw = Fn.zip(head, tail)

        val tree = pw.explain.text_hierarchy()
        assert(tree.contains("Zipped"))
        assert(tree.contains("Blackbox"))
      }

      it("simplify preserves function") {

        val head: Fn1[Int, String] = { (v: Int) => "s" + v }
        val tail: Fn1[Long, Double] = { (v: Long) => v.toDouble }

        val pw = Fn.zip(head, tail)
        val simplified = pw.simplify

        val in = Const.Provided(5) ><: (Const.Provided(3L) ><: Args.eye)

        val result = simplified.apply(in)
        assert(result == ("s5", 3.0))
      }
    }

    describe("partialEval") {
      import ai.acyclic.prover.commons.jit.eval.PartialEvalEnv
      import ai.acyclic.prover.commons.jit.Hom.Const
      import ai.acyclic.prover.commons.jit.fixture.ZippedLike.zipped

      val arg = Const.Provided(1) ><: (Const.Provided(2L) ><: Args.eye)
      val expected = (Seq(1L, 2L, 3L), Seq(2.0, 2.1, 2.2))

      val zippedTree =
        """
          |+ Zipped
          |!-- Blackbox(fn1 <at Circuits.scala:12>)
          |!-- Blackbox(fn2 <at Circuits.scala:16>)
          |""".stripMargin

      it("both inputs NotProvided") {
        val envBothNot =
          () =>
            PartialEvalEnv(Const.NotProvided ><: (Const.NotProvided ><: Args.eye), failFast = false, onlyPure = false)
        val partial = zipped.partialEval(envBothNot)
        partial.explain.text_hierarchy().shouldBe(zippedTree)
        assert(partial.apply(arg) == expected)
      }

      it("left inputs NotProvided") {
        // left NotProvided, right provided
        val envRightProvided = () =>
          PartialEvalEnv(Const.NotProvided ><: (Const.Provided(2L) ><: Args.eye), failFast = false, onlyPure = false)
        val partial = zipped.partialEval(envRightProvided)
        partial.explain
          .text_hierarchy()
          .shouldBe(
            """
            |+ Zipped
            |!-- Blackbox(fn1 <at Circuits.scala:12>)
            |!-- Provided(List(2.0, 2.1, 2.2))
            |""".stripMargin
          )
        assert(partial.apply(arg) == expected)
      }

      it("right inputs NotProvided") {
        // left provided, right NotProvided
        val envLeftProvided =
          () =>
            PartialEvalEnv(Const.Provided(1) ><: (Const.NotProvided ><: Args.eye), failFast = false, onlyPure = false)
        val partial = zipped.partialEval(envLeftProvided)
        partial.explain
          .text_hierarchy()
          .shouldBe(
            """
            |+ Zipped
            |!-- Provided(List(1, 2, 3))
            |!-- Blackbox(fn2 <at Circuits.scala:16>)
            |""".stripMargin
          )
        assert(partial.apply(arg) == expected)
      }

      it("both inputs provided") {
        val envBothProvided = () =>
          PartialEvalEnv(Const.Provided(1) ><: (Const.Provided(2L) ><: Args.eye), failFast = false, onlyPure = false)
        val partial = zipped.partialEval(envBothProvided)
        partial.explain.text_hierarchy().shouldBe(s"- Provided(${expected.toString})")
        assert(partial.apply(arg) == expected)
        assert(partial.asConstantOrNone.get.value == expected)
      }

      it("partialEval") {
        val zipped = ZippedLike.zipped.unbox

        zipped match {
          case _: Fn.Zipped[?, ?, ?, ?, ?] => succeed
          case other                       => fail(s"Expected Fn.Zipped but got ${other.getClass}")
        }

        assertDirectEvalAndPartialEval(
          zipped,
          Const.Provided(1) ><: (Const.Provided(2L) ><: Args.eye),
          (Seq(1L, 2L, 3L), Seq(2.0, 2.1, 2.2)),
          expectConstant = true
        )
      }
    }
  }

  describe("Fork") {

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

  describe("Flatten") {

    it("basic usage") {
      val baseFn: Fn1[Int, Int] = { (v: Int) => v * 2 }
      val coerceFn: Int => Fn1[Int, String] = { (t: Int) =>
        { (v: Int) => s"v=$v, t=$t" }
      }

      val flatten = Fn.Flatten(baseFn, coerceFn)

      val arg = Const.Provided(3) ><: Args.eye
      val result = flatten.apply(arg)
      assert(result == "v=3, t=6")
    }

    it("simplify preserves behavior") {
      val baseFn: Fn1[Int, Int] = { (v: Int) => v * 2 }
      val coerceFn: Int => Fn1[Int, String] = { (t: Int) =>
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

  describe("Const") {

    it("embedded fixture node keeps the same value under partial evaluation") {

      val provided = ConstLike.s1

      val direct = provided.apply(Args.eye)
      val partial = provided.partialEval(fullyProvidedEnv(Args.eye))

      assert(direct == Circuits.fn0)
      assert(partial.apply(Args.eye) == direct)
      assert(partial.asConstantOrNone.get.value == direct)
    }
  }

  describe("Cached") {

    it("evaluates only once for identical inputs") {
      val (counter, cachedFn) = ai.acyclic.prover.commons.jit.fixture.Cached.createCachedFn()

      val arg1 = Const.Provided(42) ><: Args.eye
      val arg2 = Const.Provided(42) ><: Args.eye
      val argOther = Const.Provided(99) ><: Args.eye

      // First evaluation
      assert(cachedFn.apply(arg1) == "value:42")
      assert(counter.get() == 1)

      // Second evaluation with identical input
      assert(cachedFn.apply(arg2) == "value:42")
      assert(counter.get() == 1) // Counter should not increase

      // Evaluation with different input
      assert(cachedFn.apply(argOther) == "value:99")
      assert(counter.get() == 2) // Counter increases for new input
    }

    it("getExisting returns only evaluated results (like CachedOnly)") {
      val (counter, cachedFn) = ai.acyclic.prover.commons.jit.fixture.Cached.createCachedFn()

      val arg1 = Const.Provided(42) ><: Args.eye
      val argOther = Const.Provided(99) ><: Args.eye

      // Initially empty
      assert(cachedFn.getExisting(arg1) == None)
      assert(cachedFn.getExisting(argOther) == None)

      // Evaluate arg1
      assert(cachedFn.apply(arg1) == "value:42")

      // Now getExisting should return Some for arg1, None for argOther
      assert(cachedFn.getExisting(arg1) == Some("value:42"))
      assert(cachedFn.getExisting(argOther) == None)

      // Counter should be 1
      assert(counter.get() == 1)
    }

    it("shares the same cache when invoked through Function1View") {
      val (counter, cachedFn) = ai.acyclic.prover.commons.jit.fixture.Cached.createCachedFn()

      val naturalView: Int => String = cachedFn
      val arg = Const.Provided(42) ><: Args.eye

      assert(naturalView(42) == "value:42")
      assert(counter.get() == 1)
      assert(cachedFn.getExisting(arg) == Some("value:42"))

      assert(cachedFn.apply(arg) == "value:42")
      assert(counter.get() == 1)
    }
  }

}
