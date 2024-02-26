package ai.acyclic.prover.meta2.congr

class Congruent[X, Y](val equality: X =:= Y = null.asInstanceOf[X =:= Y])

object Congruent extends CongruentSupport {

  import scala.language.experimental.macros

  implicit def lemma[A, B]: Congruent[A, B] = macro CongruentM.deriveCongruent[A, B]

}
