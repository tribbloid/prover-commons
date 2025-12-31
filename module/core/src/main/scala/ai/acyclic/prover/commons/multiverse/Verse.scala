package ai.acyclic.prover.commons.multiverse

trait Verse extends Serializable {
  // base of multiversal equality, normalise & product

  override def toString: String = this.getClass.getSimpleName
}
