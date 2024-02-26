package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait DefinedAtMixin extends Serializable {
  // TODO: need 2 implementations: compile-time (use staging) & run-time (use stacktrace)

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
