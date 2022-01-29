package ai.acyclic.graph.commons.reflect

import scala.reflect.api.Universe

trait HasUniverse {

  val universe: Universe

  final type UU = Universe with universe.type
  final lazy val getUniverse: UU = universe

  type TypeTag[T] = universe.TypeTag[T]
  type WeakTypeTag[T] = universe.WeakTypeTag[T]
  type Type = universe.Type
  type Symbol = universe.Symbol

  def rootMirror: universe.Mirror = universe.rootMirror

  def mirror: universe.Mirror = rootMirror

  trait ApiView[T] {

    def self: T
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
