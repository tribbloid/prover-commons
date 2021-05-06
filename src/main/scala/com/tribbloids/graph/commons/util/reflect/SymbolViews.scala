package com.tribbloids.graph.commons.util.reflect

trait SymbolViews extends HasUniverse {

  case class SymbolView(
      self: universe.Symbol
  ) {

    lazy val ownerOpt: Option[universe.Symbol] = {

      val owner = self.owner

      if (owner == universe.NoSymbol) None
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

    lazy val packagePrefix: String = packageChain
      .map { ss =>
        ss.name.encodedName
      }
      .reverse
      .mkString(".")

    def packagePrefixDot: String = packagePrefix + "."

    lazy val fullString: String = self.fullName

    override def toString: String = fullString
  }
}
