package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.debug.SrcDefinition.WithCode
import ai.acyclic.prover.commons.multiverse.{CanEqual, Projection}

import java.io.File
import java.util.UUID

sealed trait SrcDefinition extends Serializable with Projection.Equals {
  // TODO: should it be "SrcPosition"?

  def fileName: String

  lazy val shortFileName: String = {
    // Need to escape File.separator for regex use, especially on Windows where it's '\'
    val separatorRegex = java.util.regex.Pattern.quote(File.separator)
    fileName.split(separatorRegex).last
  }

  def lineNumber: Int

  def methodName: String

  object AtLine extends Serializable {

    lazy val short: String = { s"${shortFileName}:${lineNumber}" }
    lazy val long: String = { s"${fileName}:${lineNumber}" }
  }

  lazy val shortText = s"${methodName} <at ${AtLine.short}>"
  lazy val longText: String = shortText

  override def toString: String = { shortText }

  def withCode(codeText: String): WithCode = {
    WithCode(this, codeText = codeText)
  }

  {
    canEqualProjections += CanEqual.Native.on((fileName, lineNumber, methodName))
  }
}

object SrcDefinition extends SrcDefinition_Imp0 {

  case class Unknown(uuid: UUID) extends SrcDefinition {

    override def fileName: String = "<unknown>"

    override def lineNumber: Int = 0

    override def methodName: String = ""
  }

  // only here as a backup, should use CompileTime in most cases for speed
  case class RuntimeCallStack(
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

  class Inlined(
      _fileName: _FileName,
      _line: sourcecode.Line,
      _name: sourcecode.Name
  ) extends SrcDefinition {

    override val fileName: String = _fileName.value

    override val lineNumber: Int = _line.value

    override val methodName: String = _name.value
  }

  case class WithCode(
      raw: SrcDefinition,
      codeText: String
  ) extends SrcDefinition {

    override def fileName: String = raw.fileName

    override def lineNumber: Int = raw.lineNumber

    override def methodName: String = raw.methodName

    override lazy val longText: String = {
      Seq(shortText, codeText).mkString("\n")
    }
  }

}
