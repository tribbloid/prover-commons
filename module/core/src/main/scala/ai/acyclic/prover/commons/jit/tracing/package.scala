package ai.acyclic.prover.commons.jit
import ai.acyclic.prover.commons.jit.hom.Hom.:=>

package object tracing extends FnCanChain {

  type Input[O] = Expr.Gt[O, O]

  type TracingFnLike[-P, -I, +O] = Expr.Gt[P, I :=> O]

  type TracingFn[-I, +O] = Expr.Static[I :=> O]

  object TracingFn extends Serializable {

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

    def apply[I, O](concrete: I :=> O): TracingFn[I, O] = Const(concrete)

    case class Unary[I, O](
        proto: Input[I] :=> Expr[O]
    ) extends TracingFn[I, O] {

      val execute: I :=> O = { // as simple as possible, no runtime tracing or profiling
        :=>.at[I] { v =>
          val const = Const(v)
          val result = proto(const)
          result.reify
        }(proto.definedAt)
      }

      override val concrete: I :=> O = {
        execute
      }
    }

    case class Impl[Inputs, II, O](
        proto: Inputs :=> O,
        canReifyMany: CanReifyMany.Aux[Inputs, II]
    ) extends TracingFn[II, O] {

      override val concrete: II :=> O = {
        :=>.at[II] { ii =>
          val inputs = canReifyMany.const(ii)
          proto(inputs)
        }(proto.definedAt)
      }
    }
  }
}
