package ai.acyclic.prover.commons.function.hom

import scala.language.implicitConversions

trait HasPoly extends HasPolyLike {

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[FnCompat]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    *
    * obviously, both [[HasMono.MonoLike]] and [[FnCompat]] are its trivial examples that only has 1 case
    */
  trait Poly extends PolyLike {
    // TODO: all these cases can only be summoned when Poly is path-dependent, is there an API that works otherwise?

    def apply[I](v: I)(
        implicit
        _case: At[I]
    ): _case.Out = {
//      val revoked: FnCompat[v.type, R] = Capabilities.revokeAll[FnCompat[v.type, R], IsCase.type](_case)
//      val revoked: FnCompat[v.type, R] = Capabilities.revokeAll(_case)
      _case.apply(v)
    }

  }

  object Poly {}

  implicit def asShapelessPoly1(v: Poly): v.asShapelessPoly1.type = v.asShapelessPoly1

  implicit class PolyOps[P <: Poly](self: P) extends Serializable {}
}
