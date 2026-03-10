package ai.acyclic.prover.commons.util

import scala.reflect.ClassTag

trait HasPhantom extends HasStatic {

  /**
    * static object that should never be instantiated
    *
    * used to circumvent lack of type projection in Scala 3, for example:
    *
    *   - Before: `type T = S#Dep`
    *   - After: `val s: S = Phantom(); type T = s.Dep`
    *
    * this can make some generic type declaration shorter: e.g. `T |- R` and `T |-\- R` can be grouped into *
    * `ForAll[T] { |-[R]; |-\-[R] }`, where `ForAll extends Case` * instances:
    *
    *   - should only contain dependent types, no method or property is allowed
    *   - method or property can exist in extension view
    */
  type Phantom = Phantom.Case

  object Phantom extends StaticGroup {

    implicit def summon[T <: Case]: T = {
      throw new UnsupportedOperationException(
        "Phantom term should only be declare as lazy val for its dependent types, it should never be initialized"
      )
    }

    /**
      * create a concrete instance of [[T]] using JVM reflection and the given [[ClassTag]]
      *
      * This can be used to instantiate [[Phantom]] values that define methods and properties apart from dependent
      * types.
      *
      * always does the following:
      *
      *   - identify if [[T]] is a class or an object type
      *     - if a class: find the nullary constructor of the class and use it to create a new instance, throw a runtime
      *       exception if not found
      *     - if an object: return the object itself
      *   - cast it into [[T]]
      */
    def summonConcrete[T <: Phantom](
        ev: ClassTag[T]
    ): T = {
      val runtimeClass = ev.runtimeClass

      val instance =
        summonObject(runtimeClass).getOrElse {
          val constructor =
            try {
              runtimeClass.getDeclaredConstructor()
            } catch {
              case err: NoSuchMethodException =>
                throw new IllegalArgumentException(
                  s"Cannot instantiate phantom ${runtimeClass.getName}: nullary constructor not found",
                  err
                )
            }

          constructor.setAccessible(true)

          try {
            constructor.newInstance()
          } catch {
            case err: ReflectiveOperationException =>
              throw new IllegalStateException(
                s"Cannot instantiate phantom ${runtimeClass.getName}",
                err
              )
          }
        }

      ev.unapply(instance).getOrElse {
        throw new ClassCastException(
          s"Instantiated ${instance.getClass.getName} is not compatible with ${runtimeClass.getName}"
        )
      }
    }

    private def summonObject(runtimeClass: Class[?]): Option[Object] = {
      summonStaticObject(runtimeClass).orElse {
        val accessorName = runtimeClass.getSimpleName.stripSuffix("$")

        Option(runtimeClass.getEnclosingClass).flatMap { enclosingClass =>
          summonObject(enclosingClass).flatMap { outerInstance =>
            val accessor =
              try {
                Some(enclosingClass.getDeclaredMethod(accessorName))
              } catch {
                case _: NoSuchMethodException => None
              }

            accessor.map { method =>
              method.setAccessible(true)

              try {
                method.invoke(outerInstance)
              } catch {
                case err: ReflectiveOperationException =>
                  throw new IllegalStateException(
                    s"Cannot instantiate phantom ${runtimeClass.getName}",
                    err
                  )
              }
            }
          }
        }
      }.orElse {
        summonPackageObject(runtimeClass)
      }
    }

    private def summonStaticObject(runtimeClass: Class[?]): Option[Object] = {
      val moduleField =
        try {
          Some(runtimeClass.getField("MODULE$"))
        } catch {
          case _: NoSuchFieldException =>
            try {
              Some(runtimeClass.getDeclaredField("MODULE$"))
            } catch {
              case _: NoSuchFieldException => None
            }
        }

      moduleField.map { field =>
        field.setAccessible(true)

        try {
          field.get(null)
        } catch {
          case err: ReflectiveOperationException =>
            throw new IllegalStateException(
              s"Cannot instantiate phantom ${runtimeClass.getName}",
              err
            )
        }
      }
    }

    private def summonPackageObject(runtimeClass: Class[?]): Option[Object] = {
      val accessors = moduleAccessorPath(runtimeClass)

      accessors.headOption.flatMap { firstAccessor =>
        val packageClass =
          try {
            Some(Class.forName(s"${runtimeClass.getPackageName}.package"))
          } catch {
            case _: ClassNotFoundException => None
          }

        packageClass
          .flatMap(invokeStaticAccessor(_, firstAccessor, runtimeClass))
          .flatMap { outerInstance =>
            accessors.tail.foldLeft(Option(outerInstance)) { (current, accessor) =>
              current.flatMap(invokeInstanceAccessor(_, accessor, runtimeClass))
            }
          }
          .filter(runtimeClass.isInstance)
      }
    }

    private def moduleAccessorPath(runtimeClass: Class[?]): List[String] = {
      @annotation.tailrec
      def loop(current: Class[?], acc: List[String]): List[String] = {
        val name = current.getSimpleName

        if (!name.endsWith("$")) {
          acc
        } else {
          val updated = name.stripSuffix("$") :: acc

          Option(current.getEnclosingClass) match {
            case Some(enclosingClass) if enclosingClass.getSimpleName.endsWith("$") =>
              loop(enclosingClass, updated)
            case _ =>
              updated
          }
        }
      }

      loop(runtimeClass, Nil)
    }

    private def invokeStaticAccessor(
        ownerClass: Class[?],
        accessorName: String,
        runtimeClass: Class[?]
    ): Option[Object] = {
      findAccessor(ownerClass, accessorName).map { method =>
        method.setAccessible(true)

        try {
          method.invoke(null)
        } catch {
          case err: ReflectiveOperationException =>
            throw new IllegalStateException(
              s"Cannot instantiate phantom ${runtimeClass.getName}",
              err
            )
        }
      }
    }

    private def invokeInstanceAccessor(
        ownerInstance: Object,
        accessorName: String,
        runtimeClass: Class[?]
    ): Option[Object] = {
      findAccessor(ownerInstance.getClass, accessorName).map { method =>
        method.setAccessible(true)

        try {
          method.invoke(ownerInstance)
        } catch {
          case err: ReflectiveOperationException =>
            throw new IllegalStateException(
              s"Cannot instantiate phantom ${runtimeClass.getName}",
              err
            )
        }
      }
    }

    private def findAccessor(ownerClass: Class[?], accessorName: String) = {
      try {
        Some(ownerClass.getMethod(accessorName))
      } catch {
        case _: NoSuchMethodException =>
          try {
            Some(ownerClass.getDeclaredMethod(accessorName))
          } catch {
            case _: NoSuchMethodException => None
          }
      }
    }
  }

}
