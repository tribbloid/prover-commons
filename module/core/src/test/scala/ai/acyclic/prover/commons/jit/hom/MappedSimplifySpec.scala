//package ai.acyclic.prover.commons.jit.hom
//
//import ai.acyclic.prover.commons.jit.Hom.{Const, Fn}
//import ai.acyclic.prover.commons.jit.eval.Args
//import ai.acyclic.prover.commons.testlib.BaseSpec
//import Args.{><:, T0}
//
//class MappedSimplifySpec extends BaseSpec {
//
//  private def unaryInput(v: Int): Int ><: T0 = Const.Provided(v) ><: Args.eye
//
//  private def unarySchema[A]: Args { type Peer <: A ><: T0 } =
//    Const.NotProvided ><: Args.eye
//
//  describe("Fn.Mapped.simplify") {
//
//    it("folds left identity component") {
//
//      val left: Fn[Int ><: T0, Int] = new Fn.Identity[Int]() {
//        override lazy val inputSchema: Args { type Peer <: Int ><: T0 } = unarySchema[Int]
//      }
//
//      val right: Fn[Int ><: T0, Int] = new Fn.Impl1[Int, Int] {
//        override lazy val inputSchema: Args { type Peer <: Int ><: T0 } = unarySchema[Int]
//
//        override def apply(arg: Int ><: T0): Int = arg.head.compute + 10
//
//        override def productArity: Int = 0
//        override def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)
//      }
//
//      val mapped = new Fn.Mapped[Int ><: T0, Int, Int](left, right) {
//        override lazy val inputSchema: Args { type Peer <: Int ><: T0 } = unarySchema[Int]
//      }
//
//      val simplified = mapped.simplify
//
//      assert(!simplified.explain.nodeText.startsWith("Mapped"))
//      assert(simplified(unaryInput(5)) == 15)
//    }
//
//    it("folds right identity component") {
//
//      val left: Fn[Int ><: T0, Int] = new Fn.Impl1[Int, Int] {
//        override lazy val inputSchema: Args { type Peer <: Int ><: T0 } = unarySchema[Int]
//
//        override def apply(arg: Int ><: T0): Int = arg.head.compute * 3
//
//        override def productArity: Int = 0
//        override def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)
//      }
//
//      val right: Fn[Int ><: T0, Int] = new Fn.Identity[Int]() {
//        override lazy val inputSchema: Args { type Peer <: Int ><: T0 } = unarySchema[Int]
//      }
//
//      val mapped = new Fn.Mapped[Int ><: T0, Int, Int](left, right) {
//        override lazy val inputSchema: Args { type Peer <: Int ><: T0 } = unarySchema[Int]
//      }
//
//      val simplified = mapped.simplify
//
//      assert(!simplified.explain.nodeText.startsWith("Mapped"))
//      assert(simplified(unaryInput(5)) == 15)
//    }
//  }
//}
