package ai.acyclic.prover.commons.meta

import ai.acyclic.prover.commons.multiverse.rewrite.Delegating

import scala.reflect.api.Universe

private[meta] trait HasUniverse {

  lazy val universe: Universe & Singleton = ???

  final type UU = Universe & Singleton & universe.type
  final def getUniverse: UU = universe

  type Mirror = universe.Mirror

  type TypeTag[T] = universe.TypeTag[T]
  type WeakTypeTag[T] = universe.WeakTypeTag[T]
  type Type = universe.Type

  type Symbol = universe.Symbol

  def rootMirror: universe.Mirror = universe.rootMirror

  def mirror: universe.Mirror = rootMirror

  trait ApiView[T] extends Delegating[T] {

    val outer: HasUniverse.this.type = HasUniverse.this

    //      comment: Option[String] = None // TODO: useless?

    def _copy(self: T): ApiView[T]

    def canonicalName: String
    override def toString: String = canonicalName

    trait Breadcrumbs {

      def getCanonicalName(v: T): String = ApiView.this._copy(v).canonicalName

      case class BreadcrumbView(
          list: List[T]
      ) {

        def rightOpt: Option[T] = list.headOption
        def leftOpt: Option[T] = list.lastOption

        lazy val prefixName: String = rightOpt.map(getCanonicalName).getOrElse("")

        lazy val simpleName: String = {
          canonicalName.stripPrefix(prefixName).stripPrefix(".").stripPrefix("#")
        }

//        def validate(): Unit = {
//
//          require(
//            canonicalName.startsWith(prefixName),
//            s"'$prefixName' is not part of '${canonicalName}'"
//          )
//
//          if (list.nonEmpty) {
//
//            for (i <- list.indices.dropRight(1)) {
//
//              require(
//                getCanonicalName(list(i)).startsWith(getCanonicalName(list(i + 1))),
//                s"'${getCanonicalName(list(i))}' is not part of '${getCanonicalName(list(i + 1))}'"
//              )
//            }
//          }
//        }
      }
    }
  }
}

object HasUniverse {

  trait Runtime extends HasUniverse {

    override lazy val universe: RuntimeUniverse = RuntimeUniverse

    // TODO: useless? what's the difference
    // Since we are creating a runtime mirror using the class loader of current thread,
    // we need to use def at here. So, every time we call mirror, it is using the
    // class loader of the current thread.
    override lazy val mirror: universe.Mirror = {

      val _classloader: ClassLoader = getClass.getClassLoader

      val result: universe.Mirror = universe
        .runtimeMirror(_classloader)

      result
    }

    case class CreateTypeTag[T](
        tpe: Type,
        mirror: Mirror
    ) {

      import CreateTypeTag.*

      // TODO: TypeCreator is not in Developer's API and usage is not recommended
      def fast: TypeTag[T] = {
        universe.TypeTag.apply(
          mirror,
          NaiveTypeCreator(tpe)
        )
      }

      // TODO: this needs improvement due to:
      // https://stackoverflow.com/questions/59473734/in-scala-2-12-why-none-of-the-typetag-created-in-runtime-is-serializable
      def slowButSerializable: TypeTag[T] = {

        val toolbox = scala.tools.reflect.ToolBox(mirror).mkToolBox()

        val tree = toolbox.parse(s"scala.reflect.runtime.universe.typeTag[$tpe]")
        val result = toolbox.eval(tree).asInstanceOf[TypeTag[T]]

        result
      }

    }

    object CreateTypeTag {

      case class NaiveTypeCreator(tpe: Type) extends reflect.api.TypeCreator {

        def apply[U <: reflect.api.Universe & Singleton](m: reflect.api.Mirror[U]): U#Type = {
          //          assert(m eq mirror, s"TypeTag[$tpe] defined in $mirror cannot be migrated to $m.")
          tpe.asInstanceOf[U#Type]
        }
      }
    }
  }
}
