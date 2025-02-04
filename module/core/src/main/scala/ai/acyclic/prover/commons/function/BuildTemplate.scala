package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.SrcDefinition

object BuildTemplate {

  trait Refinement[I, O] {

    type BuildDomains[_, _]

    def refine[i, o]: BuildDomains[i, o] // TODO: need to be tighter
    final def at[i]: BuildDomains[i, O] = refine[i, O]
    final def to[o]: BuildDomains[I, o] = refine[I, o]
  }
}

trait BuildTemplate extends BuildTemplate.Refinement[Nothing, Any] { // TODO: reenable this

  protected type BuildTarget[I, O]

  protected def build[I, O](fn: I => O)(
      implicit
      _definedAt: SrcDefinition
  ): BuildTarget[I, O]

  trait IBuildDomains[I, O] extends BuildTemplate.Refinement[I, O] { // TODO: this should be an interface
    override type BuildDomains[i, o] = BuildTemplate.this.BuildDomains[i, o]

    override def refine[i, o]: BuildDomains[i, o] = BuildTemplate.this.refine[i, o]

    def apply[o <: O](fn: I => o)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, o] = {
      BuildTemplate.this.build(fn)
    }
  }
}

//trait DomainBuilder { // TODO: can be simplified, but scala type deduction is weak
//
//  protected type Target[_, _]
//
//  def define[I, R](fn: I => R)(
//      implicit
//      _definedAt: SrcDefinition
//  ): I Target R
//
//  final def apply[I, R](fn: I => R)(
//      implicit
//      _definedAt: SrcDefinition
//  ): I Target R = define(fn)
//
//  case class RefinedBuilder[I, O]() extends Domains {
//
//    type _I = I
//    type _O = O
//
//    // TODO: these should be moved into hom package
//    type Fn = Hom.Fn[I, O]
//    type FnImpl = DomainBuilder.this.Target[I, O]
//    type FnNative = (I => O)
//
//    def at[i]: RefinedBuilder[i, O] = RefinedBuilder()
//
//    def to[o]: RefinedBuilder[I, o] = RefinedBuilder()
//    final def =>>[o]: RefinedBuilder[I, o] = to
//
//    final def define[o <: O](fn: I => o)(
//        implicit
//        _definedAt: SrcDefinition
//    ): I Target o = DomainBuilder.this.define(fn)
//
//    final def apply[o <: O](fn: I => o)(
//        implicit
//        _definedAt: SrcDefinition
//    ): I Target o = define(fn)
//  }
//
//  // similar to `at` in shapeless Poly1
//  def at[I]: RefinedBuilder[I, Any] = RefinedBuilder[I, Any]()
//}
