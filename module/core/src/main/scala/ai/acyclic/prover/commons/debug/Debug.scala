package ai.acyclic.prover.commons.debug

import scala.language.implicitConversions

object Debug {

  private lazy val LZYCOMPUTE = "$lzycompute"
  private lazy val INIT = "<init>"

  def stackTracesShowStr(
      vs: Array[StackTraceElement],
      maxDepth: Int = 1
  ): String = {
    vs.slice(0, maxDepth)
      .mkString("\n\t< ")
  }

  final private val breakpointInfoBlacklist = {
    Seq(
      this.getClass.getCanonicalName,
      classOf[Thread].getCanonicalName
    ).map(_.stripSuffix("$"))
  }

  private def breakpointInfoFilter(vs: Array[StackTraceElement]) = {
    vs.filterNot { v =>
      val className = v.getClassName
      val outerClassName = className.split('$').head
      outerClassName.startsWith("scala") ||
      breakpointInfoBlacklist.contains(outerClassName)
    }
  }

  def getBreakpointInfo(
      filterInitializer: Boolean = true,
      filterLazyCompute: Boolean = true
  ): Array[StackTraceElement] = {
    val stackTraceElements: Array[StackTraceElement] = Thread.currentThread().getStackTrace
    var effectiveElements = breakpointInfoFilter(stackTraceElements)

    if (filterInitializer) effectiveElements = effectiveElements.filter(v => !(v.getMethodName == INIT))
    if (filterLazyCompute) effectiveElements = effectiveElements.filter(v => !v.getMethodName.endsWith(LZYCOMPUTE))

    effectiveElements
  }

  case class CallStackRef(
      stack: Vector[StackTraceElement],
      below: Int = 0
  ) {

    import CallStackRef._

    lazy val effectiveStack: Vector[StackTraceElement] = stack.drop(below)

    def showStr: String = {
      stackTracesShowStr(effectiveStack.toArray)
    }

    lazy val head: StackTraceElement = effectiveStack.head

    def pop(depth: Int): CallStackRef = this.copy(below = below + depth)

    def pop(
        condition: ConditionMagnet
    ): CallStackRef = {

      val filteredIndex = effectiveStack.lastIndexWhere(
        condition
      )

      this.copy(below = below + filteredIndex + 1)
    }

    def className: String = {
      head.getClassName
    }

    lazy val fnName: String = {
      if (head.isNativeMethod) {
        className
      } else {
        val name = head.getMethodName

        if (name.startsWith("<")) {
          s"$className.$name"
        } else if (name.startsWith("$")) {
          ""
        } else {
          name
        }
      }
    }

    lazy val atLine: String = {
      s"${head.getFileName}:${head.getLineNumber}"
    }

    override lazy val toString: String = {
      s"${fnName} <at $atLine>"
    }
  }

  object CallStackRef {

    def here: CallStackRef = {

      val stackInfo_raw: Array[StackTraceElement] = getBreakpointInfo()
      CallStackRef(stackInfo_raw.toVector)
    }

    def below(
        depth: Int = 1,
        condition: ConditionMagnet = ConditionMagnet.OfPaths(Nil)
    ): CallStackRef = {

      here.pop(depth).pop(condition)
    }

    trait ConditionMagnet extends (StackTraceElement => Boolean) {}

    object ConditionMagnet {

      def nameIsUnderPath(name: String, path: String): Boolean = {

        (name.startsWith(path)) && {

          val nextOpt = name.stripPrefix(path).headOption

          nextOpt match {
            case None    => true
            case Some(v) => v == '.' || v == '$'
          }
        }
      }

      implicit class OfPaths(
          paths: Seq[String]
      ) extends ConditionMagnet {

        override def apply(v: StackTraceElement): Boolean = {
          paths.exists { path =>
            // TODO: doesn't work in case class name is a substring
            val matchClass = Option(v.getClassName).exists(v => nameIsUnderPath(v, path))
            val matchModule = Option(v.getModuleName).exists(v => nameIsUnderPath(v, path))
            matchClass || matchModule
          }
        }
      }

      implicit def ofClass(
          classes: Seq[Class[_]]
      ): ConditionMagnet = {
        OfPaths(classes.map(_.getName))
      }
    }
  }

  def liftCamelCase(str: String): String = str.head.toUpper.toString + str.substring(1)
  def toCamelCase(str: String): String = str.head.toLower.toString + str.substring(1)
}
