package ai.acyclic.prover.meta2

package object congr {

  /**
    * congruence lemma in Scala 3:
    *
    * if evidence for [[Congruent[X, Y]] is present, then all terms (x: X) and (y: Y) should be considered equal, their
    * path dependent types should also be considered equal
    *
    * this feature cannot be implemented in idiomatic way (see
    * https://stackoverflow.com/questions/77663993/in-scala-3-should-2-dependent-types-that-depends-on-2-equal-singleton-objects),
    * and rely on macro / Scala 3 staging.
    */
  type ===[X, Y] = Congruent[X, Y]
}
