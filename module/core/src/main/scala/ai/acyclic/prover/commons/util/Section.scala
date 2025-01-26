//package ai.acyclic.prover.commons.util
//
//import scala.language.implicitConversions
//
///**
//  * intend to mimic the `section` keyword of LEAN 4 & assuming the lambdaP2 vs CoC conjecture, this trait only serves to
//  * group shared type arguments of its dependent types.
//  *
//  * e.g. `T |- R` and T |-\-R` can be grouped into `ForAll[T] { |-[R]; |-\-[R] }`, where `ForAll extends
//  * Section[ForAll]`
//  *
//  * section instances:
//  *   - cannot have subtypes
//  *   - summoning are automatically memoized by type argument (not implemented)
//  *   - always have dependent types that can be freely cast into the same dependent type of another instance (not
//  *     implemented)
//  */
//trait Section[Self <: Section[Self]] {
//
//  implicit def peerIsEqual(peer: Self): this.type = peer.asInstanceOf[this.type]
//}
//
//object Section {
//
//  // TODO: this feature is blocked by a language feature:
//  //  https://stackoverflow.com/questions/77663993/in-scala-3-should-2-dependent-types-that-depends-on-2-equal-singleton-objects
////  implicit def allPeersAreEqual[
////      Self <: Section[Self],
////      S1 <: Self with Singleton,
////      S2 <: Self with Singleton
////  ]: (S1 =:= S2) =
////    ???
//}
