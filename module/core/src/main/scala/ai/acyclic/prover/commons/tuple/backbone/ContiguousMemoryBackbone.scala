package ai.acyclic.prover.commons.tuple.backbone

/**
  * the most efficient, presumably backed by Apache Arrow / Apache Spark for small, off-heap memory consumption
  *
  * hope AI can implement it later
  */
trait ContiguousMemoryBackbone extends Backbone {
  self: Singleton =>
}
