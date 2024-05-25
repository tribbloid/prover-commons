package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.CallStackRef

trait SrcTraceable extends Serializable {

  {
    definedAt
  }
  final lazy val definedAt: SrcPosition = _definedAt

  protected def _definedAt: SrcPosition = {

    val thisClass = classOf[SrcTraceable]
    val stack = CallStackRef
      .below(
        1,
        condition = { v =>
          v.isUnderClasses(thisClass)
        }
      )
      .pop { v =>
        v.isLazyCompute || v.isInit
      }

    SrcPosition.^(stack)
  }
}

object SrcTraceable {

//  trait DefinedBelow {
//
//    implicit def definedHere: CallStackRef = CallStackRef
//      .below(condition = { v =>
//        v.isUnderClasses(DefinedBelow.this.getClass) || v.isArgDefault
//      })
//      .below(1)
//  }
}
