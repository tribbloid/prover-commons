package ai.acyclic.prover.commons.debug

case class CallStackRef(
    stack: Vector[StackTraceElement],
    belowIndex: Int = 0
) {

  import CallStackRef.*

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

    effectiveStack.map { v =>
      v -> condition(ElementView(v))
    }

    val belowI = effectiveStack.lastIndexWhere { v =>
      condition(ElementView(v))
    } + 1

    this.copy(belowIndex = belowIndex + belowI)
  }
}

object CallStackRef {

  lazy val LZYCOMPUTE = "$lzycompute"
  lazy val INIT = "<init>"
  lazy val STATIC_CLASS_INIT = "<clinit>"
  lazy val ARG_DEFAULT = "$default$"
  lazy val LAMBDA = "$Lambda$"

  def here: CallStackRef = {

    val stackInfo_raw: Array[StackTraceElement] = BreakpointInfo.getBreakpointInfo
    CallStackRef(stackInfo_raw.toVector)
      .below { v =>
        v.isUnderClasses(CallStackRef.getClass)
      }
  }

  def below(
      depth: Int = 0,
      condition: ElementView => Boolean = { _ =>
        false
      }
  ): CallStackRef = {

    val belowDepth = here.below(depth)

    val belowCondition = belowDepth
      .below(condition)
      .pop { v =>
        v.isArgDefault
      }

    belowCondition
  }

  case class ElementView(
      self: StackTraceElement
  ) {

    def isArgDefault: Boolean = {

      self.getMethodName.contains(ARG_DEFAULT)
    }

    def isInit: Boolean = self.getMethodName == INIT

    def isStaticClassInit = self.getMethodName == STATIC_CLASS_INIT

    def isConstructor: Boolean = isInit || isStaticClassInit // alias

    def isLambda: Boolean = {
      self.getMethodName contains LAMBDA
    }

    def isLazyCompute: Boolean = {
      self.getMethodName.endsWith(LZYCOMPUTE)
    }

    def isUnder(
        paths: Seq[String] = Nil,
        classes: Seq[Class[?]] = Nil
    ): Boolean = {

      val _paths = paths ++ classes.map(_.getName.stripSuffix("$"))

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
        classes: Class[?]*
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
