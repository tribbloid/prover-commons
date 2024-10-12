//package ai.acyclic.prover.commons.util
//
//import ai.acyclic.prover.commons.cap.Capabilities
//
//trait CanExplain extends SrcTraceable {
//
//  @transient lazy val explain: Explain = Explain(this)
//
//  override def toString: String = {
//    explain.nodeText
//  }
//}
//
///**
//  * can mixin [[Capabilities.Capability]], but so far, the only [[Capability]] is for refining candidates of polymorphic
//  * cases
//  */
//object CanExplain extends Capabilities {
//  // TODO: can this be generalised, not just for functions?
//  // TODO: it makes more sense as a type class, defaults to explaining structure of product
//
////  trait Composite extends SrcExplainable {
////
////    def composedFrom: Seq[SrcExplainable]
////  }
////
////  trait Composite1 extends Composite {
////
////    def backbone: SrcExplainable
////
////    @transient final lazy val composedFrom = Seq(backbone)
////  }
//
////  trait DecodedName extends SrcExplainable {}
//}
