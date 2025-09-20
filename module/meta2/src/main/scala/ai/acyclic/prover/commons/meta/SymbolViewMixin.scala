package ai.acyclic.prover.commons.meta

import scala.collection.mutable

private[meta] trait SymbolViewMixin extends HasUniverse {
  self: ITyper =>

  case class SymbolView(
      unbox: universe.Symbol
  ) extends ApiView[universe.Symbol] {

    lazy val ownerOpt: Option[universe.Symbol] = {

      val owner = unbox.owner

      if (owner == universe.NoSymbol) None
      else Some(owner)
    }

    object Owners extends Breadcrumbs {

      lazy val allInternal: BreadcrumbView = {

        val chain = ownerOpt.toList.flatMap { owner =>
          val v1: List[universe.Symbol] = List(owner)
          val v2: List[universe.Symbol] = copy(owner).Owners.allInternal.list

          v1 ++ v2
        }

        BreadcrumbView(chain)
      }

      lazy val all: BreadcrumbView = {
        val chain: List[universe.Symbol] = allInternal.list.filterNot { owner =>
          owner.fullName == ROOT
        }
        val result = BreadcrumbView(chain)
//        result.validate()
        result
      }

      lazy val packages: BreadcrumbView = {

        val list = all.list.reverse.takeWhile { owner =>
          owner.isPackage
        }.reverse

        BreadcrumbView(list)
      }

      lazy val static: BreadcrumbView = {

        val list = all.list.reverse.takeWhile { owner =>
          owner.isStatic
        }.reverse

        BreadcrumbView(list)
      }
    }

    override lazy val canonicalName: String = unbox.fullName

    override def _copy(self: universe.Symbol): SymbolViewMixin.this.SymbolView = copy(self)
  }

  val symbolCache = mutable.Map.empty[Symbol, SymbolView]

  def symbolView(ss: Symbol): SymbolView = symbolCache.getOrElseUpdate(
    ss,
    SymbolView(ss)
  )
}
