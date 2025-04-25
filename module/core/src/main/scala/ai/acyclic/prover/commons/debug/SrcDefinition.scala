package ai.acyclic.prover.commons.debug

import ai.acyclic.prover.commons.multiverse.{CanEqual, Projection}

import java.io.File
import java.util.UUID

sealed trait SrcDefinition extends Serializable with Projection.Equals {
  // TODO: can use Lihaoyi's sourcecode library

  def fileName: String

  lazy val shortFileName: String = fileName.split(File.separator).last

  def lineNumber: Int

  def methodName: String

  object AtLine extends Serializable {

    lazy val short: String = { s"${shortFileName}:${lineNumber}" }
    lazy val long: String = { s"${fileName}:${lineNumber}" }
  }

  lazy val shortText = s"${methodName} <at ${AtLine.short}>"
  lazy val longText = s"${methodName} <at ${AtLine.long}>"

  override def toString: String = { shortText }

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

  private type _FileName = sourcecode.File

  case class Inlined()(
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
  ): Inlined = Inlined()

}
