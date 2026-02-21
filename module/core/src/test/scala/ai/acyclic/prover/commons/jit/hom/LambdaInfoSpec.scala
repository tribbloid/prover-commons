package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.jit.eval.Args.{><:, T0}

class LambdaInfoSpec extends BaseSpec {

  import LambdaInfoSpec._

  it("should extract outer from a lambda defined in a class") {
    val outer = new OuterClass(10)
    val fn = outer.getLambda
    val info = LambdaInfo(fn)
    assert(info.isDefined)
    assert(info.get.outer.contains(outer))
  }

  it("should extract outer from a lambda defined in a class (generic)") {
    val outer = new OuterGenericClass("hello")
    val fn = outer.getLambda
    val info = LambdaInfo(fn)
    assert(info.isDefined)
    assert(info.get.outer.contains(outer))
  }

  it("should NOT extract outer from a lambda defined in an object") {
    val fn = OuterObject.getLambda
    val info = LambdaInfo(fn)
    assert(info.isDefined)
    assert(info.get.outer.isEmpty)
  }

  it("should be defined for a closure") {
    val captured = 42
    val fn: Hom.Fn[Int ><: T0, Int] = { (x: Int) => x + captured }

    fn match {
      case bb: Hom.Fn.Blackbox[_, _] =>
        assert(bb.lambdaInfo.isDefined)
        assert(bb.lambdaInfo.get.freeVariables.contains(captured))
      case _ => fail("Required Blackbox")
    }
  }

  it("should be empty for a named class") {
    class NamedFn extends (Int => Int) {
      def apply(v1: Int): Int = v1
    }
    val fn: Hom.Fn[Int ><: T0, Int] = new NamedFn

    fn match {
      case bb: Hom.Fn.Blackbox[_, _] =>
        assert(bb.lambdaInfo.isEmpty)
      case _ => fail("Required Blackbox")
    }
  }
}

object LambdaInfoSpec {

  class OuterClass(val v: Int) {
    def getLambda: Int => Int = { x =>
      x + v
    }
  }

  class OuterGenericClass[T](val v: T) {
    def getLambda: Int => String = { x =>
      v.toString + x
    }
  }

  object OuterObject {
    val v = 3
    def getLambda: Int => Int = { x =>
      x + v
    }
  }
}
