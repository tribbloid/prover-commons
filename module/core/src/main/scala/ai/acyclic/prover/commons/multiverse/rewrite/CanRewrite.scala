package ai.acyclic.prover.commons.multiverse.rewrite

import ai.acyclic.prover.commons.multiverse.Verse

trait CanRewrite[-T] extends Verse {

  def rewrite(v: T): Seq[Any]
}
