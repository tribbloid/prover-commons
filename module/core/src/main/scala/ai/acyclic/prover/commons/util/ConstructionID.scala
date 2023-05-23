package ai.acyclic.prover.commons.util

object ConstructionID {

  protected object Impl extends (Any => Int) {
    override def apply(v1: Any): Int = System.identityHashCode(v1)
  }

  def apply[T]: T => Int = Impl.asInstanceOf[T => Int]

}
