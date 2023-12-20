package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait DefinedAtMixin extends Serializable {

  private val callStackRef: CallStackRef = {
//    if (this.isInstanceOf[Singleton]) {
//      // already at the definition site, no need to black itself
//      CallStackRef(blacklistedClasses = Seq(classOf[DefinedAtMixin]))
//    } else {

    CallStackRef(1, blacklistedClasses = Seq(this.getClass))
//    }

  }

  lazy val definedAt: String = {
    s"${callStackRef.head.getFileName}:${callStackRef.head.getLineNumber}"
  }

}
