package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.tracing.Tracer.Endo

trait TracerCanChain {

  abstract class CanChain[-T](
      implicit
      defAt: SrcDefinition
  ) {

    type Repr
    def parse(out: T): Repr
  }

  trait CanChain_Imp1 {
    self: CanChain.type =>

    implicit def forConst[T]: T Repr T = new CanChain[T] {
      type Repr = T
      def parse(out: T): Repr = ???
    }
  }

  trait CanChain_Imp0 extends CanChain_Imp1 {
    self: CanChain.type =>

    implicit def forEndo[T]: Endo[T] Repr T = new CanChain[Endo[T]] {
      type Repr = T
      def parse(out: Endo[T]): Repr = ???
    }
  }

  object CanChain extends CanChain_Imp0 {

    infix type Repr[T, R] = CanChain[T] { type Repr = R }

    implicit def forTuple2[L, R](
        implicit
        forL: CanChain[L],
        forR: CanChain[R]
    ): (L, R) Repr (forL.Repr, forR.Repr) = new CanChain[(L, R)] {

      override type Repr = (forL.Repr, forR.Repr)
      override def parse(out: (L, R)): (forL.Repr, forR.Repr) = ???
    }
  }

}
