package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.FnLike.Transparent1

abstract class FnSystem extends HasMorphism with HasPoly with Serializable {

  implicit class morphismIsPoly[
      T_/\
  ](val reference: Morphism[T_/\])
      extends Poly
      with Transparent1 {

    import reference._

    implicit def _onlyCase[T <: T_/\]: Case[FnCompat[In[T], Out[T]]] = {
      at[In[T]].apply[Out[T]] { arg =>
        val result: Out[T] = reference.apply[T](arg)
        result
      }
    }
  }
}
