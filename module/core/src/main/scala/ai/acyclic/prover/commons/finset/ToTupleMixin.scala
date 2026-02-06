package ai.acyclic.prover.commons.finset

import scala.language.implicitConversions

/**
  * formerly "ProductTuples", this was mistake
  */
trait ToTupleMixin {
  self: Finsets =>
  // TODO: don't know how to implement it efficiently yet

  object Backbone extends ToTupleBackbone {
    override type VBound = self.VBound
  }

//  implicit def unitView(v: Eye): Backbone.Eye = Backbone.Eye

//  implicit def consView[T <: Fin, H <: VBound](v: T >< H) = {
//    val t_h = deCons(v)
//    Backbone.cons(t_h._1, t_h._2)
//  }
}

object ToTupleMixin {}
