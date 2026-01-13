package ai.acyclic.prover.commons.jit.tracing

trait ConstructorCanChain {

  abstract class CanChain[-T] {

    type Repr
    def parse(out: T): Expr[Repr]
  }

  trait CanChain_Imp1 {
    self: CanChain.type =>

    implicit def forConst[T]: T Yield T = new CanChain[T] {
      type Repr = T
      def parse(out: T): Expr[T] = Const(out)
    }
  }

  trait CanChain_Imp0 extends CanChain_Imp1 {
    self: CanChain.type =>

    implicit def forTracer[T]: Expr[T] Yield T = new CanChain[Expr[T]] {
      type Repr = T
      def parse(out: Expr[T]): Expr[T] = out
    }
  }

  object CanChain extends CanChain_Imp0 {

    infix type Yield[T, R] = CanChain[T] { type Repr = R }

    implicit def forTuple2[L, R](
        implicit
        forL: CanChain[L],
        forR: CanChain[R]
    ): (L, R) Yield (forL.Repr, forR.Repr) = new CanChain[(L, R)] {

      override type Repr = (forL.Repr, forR.Repr)
      override def parse(out: (L, R)): Expr[(forL.Repr, forR.Repr)] = {

        val lExpr = forL.parse(out._1)
        val rExpr = forR.parse(out._2)

        Expr.Tuple2(lExpr, rExpr)
      }
    }
  }
}
