package ai.acyclic.prover.commons.meta

trait HasITyper extends HasUniverse {

  val reflection: ITyper
  final override lazy val universe: reflection.universe.type = reflection.universe

}
