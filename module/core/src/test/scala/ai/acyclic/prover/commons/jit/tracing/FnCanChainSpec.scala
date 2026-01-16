package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.testlib.BaseSpec

class FnCanChainSpec extends BaseSpec {

  describe("forTuple2") {

    it("should chain simple values (via forConst or forTuple2)") {
      val cc = implicitly[CanChain[(Int, Int)]]
      val resultExpr = cc.parse((1, 2))
      assert(resultExpr.getConcrete(null) == (1, 2))
    }

    it("should chain Exprs into Expr of Tuple") {
      val e1 = Const(1)
      val e2 = Const(2)

      // Explicitly use forTuple2 to ensure we test the implementation
      // We use forTracer for the components (Expr[Int])
      // forTracer[Int] returns Expr[Int] Repr Int
      val ft2 = CanChain.forTuple2(CanChain.forTracer[Int], CanChain.forTracer[Int])

      val tupleOfExprs = (e1, e2)
      val parsed = ft2.parse(tupleOfExprs)

      // Result should be Expr[(Int, Int)]
      // Logic checks:
      val value = parsed.getConcrete(null)
      assert(value == (1, 2))

      // Ensure it is not just wrapping the tuple of exprs
      // parsed.getValue returns (Int, Int), NOT (Expr[Int], Expr[Int])
      // This proves decomposition and recombination happened.
    }
  }
}
