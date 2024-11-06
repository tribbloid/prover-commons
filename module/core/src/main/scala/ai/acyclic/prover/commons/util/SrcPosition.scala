package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.CallStackRef
import ai.acyclic.prover.commons.same.Same

trait SrcPosition extends Serializable with Same.Native.IWrapper {
  // TODO: can use Lihaoyi's sourcecode library

  def fileName: String

  def lineNumber: Int

  def methodName: String

  lazy val atLine: String = {
    s"${fileName}:${lineNumber}"
  }

  override lazy val toString: String = {
    s"${methodName} <at $atLine>"
  }

  override protected def samenessKey: Any = (fileName, lineNumber, methodName)
}

object SrcPosition {

  @deprecated("use CompileTime instead")
  case class Runtime(
      stack: CallStackRef = {

        val stack = CallStackRef
          .below(
            condition = { v =>
              v.isUnderClasses(this.getClass)

            }
          )

        val filtered = stack
          .pop { v =>
            v.isLazyCompute || v.isInit
          }
        filtered
      }
  ) extends SrcPosition {

    private lazy val head = stack.head

    lazy val className: String = {
      head.getClassName
    }

    override def fileName: String = head.getFileName

    override def lineNumber: Int = head.getLineNumber

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

  private type _FileName = sourcecode.FileName

  case class CompileTime()(
      implicit
      _fileName: _FileName,
      _line: sourcecode.Line,
      _name: sourcecode.Name
  ) extends SrcPosition {

    override val fileName: String = _fileName.value

    override val lineNumber: Int = _line.value

    override val methodName: String = _name.value
  }

  implicit def default(
      implicit
      _fileName: _FileName,
      _line: sourcecode.Line,
      _name: sourcecode.Name
  ): CompileTime = CompileTime()

}
