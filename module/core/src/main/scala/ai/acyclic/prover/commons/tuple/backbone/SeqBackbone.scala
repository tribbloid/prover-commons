package ai.acyclic.prover.commons.tuple.backbone

trait SeqBackbone extends Scaffold {
  self: Singleton =>

  override type Element[V <: VBound] = V
}
