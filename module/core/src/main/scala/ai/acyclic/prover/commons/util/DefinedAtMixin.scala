package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait DefinedAtMixin extends Serializable {

  {
    definedAt
  }

  lazy val definedAt: CallStackRef = {

    val thisClass = classOf[DefinedAtMixin]
    CallStackRef
      .below(
        1,
        condition = { v =>
          v.isUnderClasses(thisClass)
        }
      )
      .pop { v =>
        v.isLazyCompute || v.isInit
      }
  }
}
