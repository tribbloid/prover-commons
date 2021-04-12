package com.tribbloids.graph.commons.util.reflect

trait SymbolViews extends HasUniverse {

  case class SymbolView(
      symbol: _universe.Symbol
  ) {

    lazy val ownerOpt: Option[_universe.Symbol] = {

      val owner = symbol.owner

      if (owner == universe.NoSymbol) None
      else Some(owner)
    }

    lazy val ownerChain: List[_universe.Symbol] = {

      ownerOpt.toList.flatMap { owner =>
        val v1: List[_universe.Symbol] = List(owner)
        val v2: List[_universe.Symbol] = copy(owner).ownerChain

        val result = v1 ++ v2
        result
      }
    }

    lazy val noRoot: List[_universe.Symbol] = ownerChain.filterNot { owner =>
      owner.fullName == "<root>"
    }

    lazy val packageChain: List[_universe.Symbol] = noRoot.reverse.takeWhile { owner =>
      owner.isPackage
    }.reverse

    lazy val packageRepr: String = packageChain
      .map { ss =>
        ss.name.encodedName
      }
      .reverse
      .mkString(".")

    def packagePrefix: String = packageRepr + "."
  }
}
