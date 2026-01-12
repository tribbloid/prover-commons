package ai.acyclic.prover.commons.congr

class Congruent[X, Y](val equality: X =:= Y = null.asInstanceOf[X =:= Y])

object Congruent extends CongruentSupport {

  inline given lemma[A, B]: Congruent[A, B] = ${ CongruentM.deriveCongruent[A, B] }

}
