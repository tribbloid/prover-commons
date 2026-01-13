package ai.acyclic.prover.meta2.meta

trait HasITyper extends HasUniverse {

  val reflection: ITyper
  final override lazy val universe: reflection.universe.type = reflection.universe

}
