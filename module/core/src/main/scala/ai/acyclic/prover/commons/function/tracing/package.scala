package ai.acyclic.prover.commons.function
import ai.acyclic.prover.commons.function.hom.Hom.:=>

package object tracing extends ConstructorImplicits {

  type ConstructorLike[P, -I, +O] = Expr.Gt[P, I :=> O]

  object ConstructorLike extends ConstructorImplicits {
    // TODO: in Scala 3 syntax, all implicits are automatically introduced to Expr
    //  I wonder if this mechanism can be abused further :->

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

  type Constructor[-I, +O] = ConstructorLike[Any, I, O]
}
