package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.CallStackRef
import ai.acyclic.prover.commons.multiverse.{CanEqual, View}

import java.util.UUID

sealed trait SrcDefinition extends Serializable with View.Equals {
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

  {
    canEqualProjections += CanEqual.Native.on((fileName, lineNumber, methodName))

  }
}

object SrcDefinition {

  case class Unknown(uuid: UUID) extends SrcDefinition {

    override def fileName: String = "<unknown>"

    override def lineNumber: Int = 0

    override def methodName: String = ""
  }

  // only here as a backup, should use CompileTime in most cases for speed
  case class Runtime(
      belowClasses: Seq[Class[?]] = Nil
  )(
      implicit
      stack: CallStackRef = {

        val stack = CallStackRef
          .below(
            condition = { v =>
              v.isUnderClasses((Seq(this.getClass) ++ belowClasses)*)
            }
          )

        val filtered = stack
          .pop { v =>
            v.isLazyCompute || v.isInit
          }
        filtered
      }
  ) extends SrcDefinition {

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
  ) extends SrcDefinition {

    override val fileName: String = _fileName.value

    override val lineNumber: Int = _line.value

    override val methodName: String = _name.value
  }

  implicit def get(
      implicit
      _fileName: _FileName,
      _line: sourcecode.Line,
      _name: sourcecode.Name
  ): CompileTime = CompileTime()

}
