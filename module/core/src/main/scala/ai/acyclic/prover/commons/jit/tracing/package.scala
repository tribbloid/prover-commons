package ai.acyclic.prover.commons.jit
import ai.acyclic.prover.commons.jit.hom.Hom.:=>

package object tracing extends FnImplicits {

  type TracingFnLike[P, -I, +O] = Expr.Gt[P, I :=> O]

  object TracingFnLike {

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

  type TracingFn[-I, +O] = TracingFnLike[Any, I, O]
}
