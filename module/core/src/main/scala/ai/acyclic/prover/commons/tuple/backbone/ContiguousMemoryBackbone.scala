package ai.acyclic.prover.commons.tuple.backbone
import ai.acyclic.prover.commons.tuple.MonoidalProds

/**
  * the most efficient, presumably backed by Apache Arrow / Apache Spark for small, off-heap memory consumption
  *
  * hope AI can implement it later
  */
trait ContiguousMemoryBackbone extends Backbone {
  self: Singleton =>
}
