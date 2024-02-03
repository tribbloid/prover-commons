package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait DefinedAtMixin extends Serializable {

  {
    definedAt
  }
  final lazy val definedAt: CallStackRef = _definedAt

  protected def _definedAt: CallStackRef = {

    val thisClass = classOf[DefinedAtMixin]
    CallStackRef
      .below(
        1,
        condition = { v =>
          v.isDefinedAtClasses(thisClass)
        }
      )
      .pop { v =>
        v.isLazyCompute || v.isInit
      }
  }
}
