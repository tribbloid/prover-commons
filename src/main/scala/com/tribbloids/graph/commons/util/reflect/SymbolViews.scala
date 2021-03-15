package com.tribbloids.graph.commons.util.reflect

trait SymbolViews extends HasUniverse {

  case class SymbolView(
      symbol: universe.Symbol
  ) {

    lazy val ownerOpt: Option[universe.Symbol] = {

      val owner = symbol.owner

      if (owner == getUniverse.NoSymbol) None
      else Some(owner)
    }

    lazy val ownerChain: List[universe.Symbol] = {

      ownerOpt.toList.flatMap { owner =>
        val v1: List[universe.Symbol] = List(owner)
        val v2: List[universe.Symbol] = copy(owner).ownerChain

        val result = v1 ++ v2
        result
      }
    }

    lazy val noRoot: List[universe.Symbol] = ownerChain.filterNot { owner =>
      owner.fullName == "<root>"
    }

    lazy val packageChain: List[universe.Symbol] = noRoot.reverse.takeWhile { owner =>
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
