package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import org.scalatest.funspec.AnyFunSpec

class CanReifyManySpec extends AnyFunSpec {

  implicit val src: SrcDefinition = SrcDefinition.Unknown(java.util.UUID.randomUUID())

  describe("CanReifyMany") {

    it("should reify Input[Int] (atom)") {
      val v1: Input[Int] = Const(1)
      val reified = implicitly[CanReifyMany[Input[Int]]].reifyMany(v1)
      assert(reified == 1)
    }

    it("should reify Tuple1[Input[Int]]") {
      val v1: Input[Int] = Const(1)
      val tuple = Tuple1(v1)
      val reifyMany = implicitly[CanReifyMany[Tuple1[Input[Int]]]]
      val reified = reifyMany.reifyMany(tuple)
      assert(reified == 1)
    }

    it("should reify (Input[Int], Input[String])") {
      val v1: Input[Int] = Const(1)
      val v2: Input[String] = Const("s")
      val tuple = (v1, v2)
      val reifyMany = implicitly[CanReifyMany[(Input[Int], Input[String])]]
      val reified = reifyMany.reifyMany(tuple)
      println(s"Tuple2 reified: $reified")
      assert(reified == (1, "s"))
    }

    it("should reify (Input[Int], Input[String], Input[Boolean])") {
      val v1: Input[Int] = Const(1)
      val v2: Input[String] = Const("s")
      val v3: Input[Boolean] = Const(true)
      val tuple = (v1, v2, v3)
      val reifyMany = implicitly[CanReifyMany[(Input[Int], Input[String], Input[Boolean])]]
      val reified = reifyMany.reifyMany(tuple)
      println(s"Tuple3 reified: $reified")
      // Zippable with right-recursive structure produces nested tuples: (1, (s, true))
      assert(reified == (1, ("s", true)))
    }
  }
}
