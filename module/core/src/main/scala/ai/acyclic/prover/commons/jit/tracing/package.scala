package ai.acyclic.prover.commons.jit
import ai.acyclic.prover.commons.jit.hom.Hom.:=>

package object tracing extends FnImplicits {

  type Input[O] = Expr.Input[O]

  type TracingFn[-P, -I, +O] = Expr.Gt[P, I :=> O]

  object TracingFn {

    //  implicit def unzipVar[I, A, B](
    //      v: Constructor[I, (A, B)]
    //  )(
    //      implicit
    //      pos: SrcDefinition
    //  ): (Constructor[I, A], Constructor[I, B]) = {
    //    val v1: Constructor[I, A] = new UnaryView(v).map(v => v.getValue._1)
    //    val v2: Constructor[I, B] = new UnaryView(v).map(v => v.getValue._2)
    //    (v1, v2)
    //  }

    // UnaryView moved to TracerImplicits

    // CAUTION: do not add Expr2[T] unless absolutely necessary
    // all reduction rules should be defined for curried form that yields higher order function(s)
  }

  type StaticTracingFn[-I, +O] = Expr.Static[I :=> O]

  object StaticTracingFn {

    case class Impl[I, O](
        proto: Input[I] :=> Expr[O]
    ) extends Expr.Static[I :=> O] {

      val execute: I :=> O = { // as simple as possible, no runtime tracing or profiling
        :=>.at[I] { v =>
          val const = Const(v)
          val result = proto(const)
          result.getConcrete
        }
      }

      override val concrete: I :=> O = {
        execute
      }
    }
  }
}
