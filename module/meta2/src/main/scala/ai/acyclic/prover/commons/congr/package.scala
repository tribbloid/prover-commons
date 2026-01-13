package ai.acyclic.prover.commons

package object congr {

  /**
    * this package should implement congruence lemma in Scala 3:
    *
    * if evidence for [[Congruent[X, Y]] is present, then all terms (x: X) and (y: Y) should be considered equal, their
    * path dependent types should also be considered equal
    *
    * if this feature cannot be implemented in idiomatic way:
    *   - search for compiler extension published online
    *   - try to implement this feature using macro / Scala 3 staging
    */

  type ===[X, Y] = Congruent[X, Y]
}
