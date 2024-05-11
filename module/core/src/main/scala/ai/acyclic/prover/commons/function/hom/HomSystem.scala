package ai.acyclic.prover.commons.function.hom

object HomSystem extends HomBase {

  override type Arg1[T] = T
  override type _Unit = Unit

  override def arg1[T](v: T): T = v
  override val _unit: Unit = ()

}
