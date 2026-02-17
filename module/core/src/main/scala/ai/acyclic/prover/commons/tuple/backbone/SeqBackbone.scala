package ai.acyclic.prover.commons.tuple.backbone

trait SeqBackbone extends Backbone {
  self: Singleton =>

  override type Element[V <: VBound] = V
}
