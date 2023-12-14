package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait DefinedAtMixin extends Serializable {

  private val callStackRef: CallStackRef = CallStackRef(blacklistedClasses = Seq(this.getClass))

  lazy val definedAt: String = s"${callStackRef.head.getFileName}:${callStackRef.head.getLineNumber}"

}
