package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.CallStackRef

trait SrcPosition {

  def fileName: String

  def lineNumber: String

  def methodName: String

  lazy val atLine: String = {
    s"${fileName}:${lineNumber}"
  }

  override lazy val toString: String = {
    s"${methodName} <at $atLine>"
  }
}

object SrcPosition {

  case class ^(
      stack: CallStackRef
  ) extends SrcPosition {

    private lazy val head = stack.head

    lazy val className: String = {
      head.getClassName
    }

    override def fileName: String = head.getFileName

    override def lineNumber: String = head.getLineNumber.toString

    override lazy val methodName: String = {

      if (head.isNativeMethod) {
        className
      } else {
        val name = head.getMethodName

        if (name.startsWith("<")) {
          s"$className.$name"
        } else {
          name.stripSuffix(CallStackRef.LZYCOMPUTE).stripSuffix("$")
        }
      }
    }
  }

  // TODO: need the second impl resolved at compile-time, after Scala 3.
  //  a good example is kyo.internal.Position
  implicit def here: SrcPosition = {

    val stack = CallStackRef
      .below(
        condition = _.isUnderClasses(this.getClass)
      )
      .pop { v =>
        v.isLazyCompute || v.isInit
      }

    ^(stack)
  }
}
