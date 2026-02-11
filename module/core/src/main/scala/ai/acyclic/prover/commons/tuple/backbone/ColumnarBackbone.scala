package ai.acyclic.prover.commons.tuple.backbone

/**
  * similar to [[ContiguousMemoryBackbone]], but backed by a columnar memory layout
  */
trait ColumnarBackbone extends Backbone {
  self: Singleton =>
}
