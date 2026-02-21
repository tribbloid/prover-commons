package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.jit.fixture.*
import ai.acyclic.prover.commons.jit.hom.Hom.Fn
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.jit.hom.Hom.{Const, Fn}
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

object FnSpec {}

class FnSpec extends BaseSpec {

  import Circuits.*

  describe("define by") {

    it("subtype") { // disabled, current compiler is janky

      case object cc extends Fn.Impl[Int ><: T0, String] {

        def apply(v: Int ><: T0): String = "" + v.head.compute
      }
      assert(
        (cc.apply(
          Args.cons(Const.Provided(1).asInstanceOf[Hom.ConstantFn[Int]], Args.eye).asInstanceOf[Int ><: T0]
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
          Args.cons(Const.Provided(1).asInstanceOf[Hom.ConstantFn[Int]], Args.eye).asInstanceOf[Int ><: T0]
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

      val cc1 = cc.asInstanceOf[Fn.Blackbox[Int, String]]

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

            val in = Args.cons(Const.Provided(1).asInstanceOf[Hom.ConstantFn[Int]], Args.eye).asInstanceOf[Int ><: T0]
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

            val in = Args.cons(Const.Provided(1).asInstanceOf[Hom.ConstantFn[Int]], Args.eye).asInstanceOf[Int ><: T0]
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

            val in = Args.cons(Const.Provided(1).asInstanceOf[Hom.ConstantFn[Int]], Args.eye).asInstanceOf[Int ><: T0]
            val r1 = fn.apply(in)
            assert(r1 == "10b")
          }
      }
    }

  }

  describe("pointwise") {

    PointwiseAndChain.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it(i.toString) {

          val normal = fn.simplify
          normal.explain
            .text_hierarchy()
            .shouldBe(
              s
            )

          val combinedIn = Args
            .cons(
              Const.Provided(1).asInstanceOf[Hom.ConstantFn[Int]],
              Args
                .cons(
                  Const.Provided(2L).asInstanceOf[Hom.ConstantFn[Long]],
                  Args.eye
                )
                .asInstanceOf[Long ><: T0]
            )
            .asInstanceOf[Int ><: Long ><: T0]

          val fnTraced = ai.acyclic.prover.commons.jit.cps.Continuation.tracingToFunction(fn).asInstanceOf[Any => Any]
          val r1 = fnTraced.apply(combinedIn)
          assert(r1 == List(3.0, 4.1, 5.2))
        }
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
