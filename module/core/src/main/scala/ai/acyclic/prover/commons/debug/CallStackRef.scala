package ai.acyclic.prover.commons.debug

case class CallStackRef(
    stack: Vector[StackTraceElement],
    belowIndex: Int = 0
) {

  import CallStackRef._

  lazy val effectiveStack: Vector[StackTraceElement] = stack.drop(belowIndex)

  def showStr: String = {
    stackTracesShowStr(effectiveStack.toArray)
  }

  lazy val head: StackTraceElement = effectiveStack.head

  def pop(
      condition: ElementView => Boolean
  ): CallStackRef = {

    val firstNotI = effectiveStack.indexWhere { v =>
      !condition(ElementView(v))
    }

    this.copy(belowIndex = belowIndex + firstNotI)
  }

  def below(depth: Int): CallStackRef = this.copy(belowIndex = belowIndex + depth)

  def below(
      condition: ElementView => Boolean
  ): CallStackRef = {

    val belowI = effectiveStack.lastIndexWhere { v =>
      condition(ElementView(v))
    } + 1

    this.copy(belowIndex = belowIndex + belowI)
  }
}

object CallStackRef {

  lazy val LZYCOMPUTE = "$lzycompute"
  lazy val INIT = "<init>"
  lazy val ARG_DEFAULT = "$default$"
  lazy val LAMBDA = "$Lambda$"

  def here: CallStackRef = {

    val stackInfo_raw: Array[StackTraceElement] = Debug.getBreakpointInfo
    CallStackRef(stackInfo_raw.toVector)
  }

  def below(
      depth: Int = 1,
      condition: ElementView => Boolean = { _ =>
        false
      }
  ): CallStackRef = {

    here
      .below(depth)
      .below(condition)
      .pop { v =>
        v.isArgDefault
      }
  }

  case class ElementView(
      self: StackTraceElement
  ) {

    def isArgDefault: Boolean = {

      self.getMethodName.contains(ARG_DEFAULT)
    }

    def isInit: Boolean = {
      self.getMethodName == INIT
    }

    def isLambda: Boolean = {
      self.getMethodName contains LAMBDA
    }

    def isLazyCompute: Boolean = {
      self.getMethodName.endsWith(LZYCOMPUTE)
    }

    def isUnder(
        paths: Seq[String] = Nil,
        classes: Seq[Class[_]] = Nil
    ): Boolean = {

      val _paths = paths ++ classes.map(_.getName)

      def nameIsUnderPath(name: String, path: String): Boolean = {

        (name.startsWith(path)) && {

          val nextOpt = name.stripPrefix(path).headOption

          nextOpt match {
            case None    => true
            case Some(v) => v == '.' || v == '$'
          }
        }
      }

      val pathsMatch = _paths.exists { path =>
        // TODO: doesn't work in case class name is a substring
        val matchClass = Option(self.getClassName).exists(v => nameIsUnderPath(v, path))
        val matchModule = Option(self.getModuleName).exists(v => nameIsUnderPath(v, path))
        matchClass || matchModule
      }

      pathsMatch
    }

    def isUnderPaths(
        paths: String*
    ): Boolean = {
      isUnder(paths = paths)
    }

    def isUnderClasses(
        classes: Class[_]*
    ): Boolean = {
      isUnder(classes = classes)
    }
  }

  def stackTracesShowStr(
      vs: Array[StackTraceElement],
      maxDepth: Int = 1
  ): String = {
    vs.slice(0, maxDepth)
      .mkString("\n\t< ")
  }

}
