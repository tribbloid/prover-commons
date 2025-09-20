package ai.acyclic.prover.commons.debug

object BreakpointInfo {

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

  def getBreakpointInfo: Array[StackTraceElement] = {

    val stackTraceElements: Array[StackTraceElement] = Thread.currentThread().getStackTrace
    val effectiveElements = breakpointInfoFilter(stackTraceElements)

    effectiveElements
  }

  def liftCamelCase(str: String): String = str.head.toUpper.toString + str.substring(1)
  def toCamelCase(str: String): String = str.head.toLower.toString + str.substring(1)

}
