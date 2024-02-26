package ai.acyclic.prover.commons.jit.hom

final case class LambdaInfo(
    /**
      * Scala compiler automatically convert a lambda definition (like {x: Int => x + 1}) into a Java function/closure,
      * with each free variable assigned as an ad-hoc object member, named "arg$1", "arg$2" etc.
      *
      * This is only the rule in JVM, other runtimes may have different rules
      */
    freeVariables: Seq[Any],
    /**
      * If lambda definition are defiend inside a class, this is a reference to the instance class
      */
    outer: Option[Any] = None
) {}

object LambdaInfo {

  def apply(fn: Any): Option[LambdaInfo] = {

    val name = fn.getClass.getName
    val isLambda = name.contains("$$Lambda") || name.contains("$$anonfun")

    if (isLambda) {

      val fields = fn.getClass.getDeclaredFields
      val freeVariables: Seq[Any] = fields.toSeq
        .filterNot(v => java.lang.reflect.Modifier.isStatic(v.getModifiers))
        .map { field =>
          field.setAccessible(true)
          field.get(fn)
        }

      val outerPrefix = name.split("\\$\\$").head

      val outer: Option[Any] =
        try {
          //          val outerClass = Class.forName(outerPrefix)
          //          freeVariables.find { v =>
          //            outerClass.isInstance(v)
          //          }
          // can't use type check, the outer class may be generic and erased
          // fortunately, the outer reference is usually the first capture
          // TODO: this is naive
          val outerClass = Class.forName(outerPrefix)
          freeVariables.find { v =>
            outerClass.isAssignableFrom(v.getClass)
          }
        } catch {
          case _: ClassNotFoundException => None
          case _: Throwable              => None
        }

      Some(new LambdaInfo(freeVariables, outer))
    } else None
  }
}
