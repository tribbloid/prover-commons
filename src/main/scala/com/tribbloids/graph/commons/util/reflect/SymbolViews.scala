package com.tribbloids.graph.commons.util.reflect

trait SymbolViews extends HasUniverse {
  self: Reflection =>

  case class SymbolView(
      self: universe.Symbol
  ) {

    lazy val ownerOpt: Option[universe.Symbol] = {

      val owner = self.owner

      if (owner == universe.NoSymbol) None
      else Some(owner)
    }

    case class PrefixChain(list: List[universe.Symbol]) {

      def prefix: String = list.reverse
        .map { v =>
          v.name.encodedName
        }
        .mkString(".")

      def shorten: String = self.fullName.stripPrefix(prefix)
    }

    lazy val allOwners: PrefixChain = {

      val chain = ownerOpt.toList.flatMap { owner =>
        val v1: List[universe.Symbol] = List(owner)
        val v2: List[universe.Symbol] = copy(owner).allOwners.list

        val result = v1 ++ v2
        result
      }

      PrefixChain(chain)
    }

    lazy val owners: PrefixChain = {
      val noRoot: List[universe.Symbol] = allOwners.list.filterNot { owner =>
        owner.fullName == "<root>"
      }
      PrefixChain(noRoot)
    }

    lazy val packages: PrefixChain = {

      val list = owners.list.reverse.takeWhile { owner =>
        owner.isPackage
      }.reverse

      PrefixChain(list)
    }

    lazy val statics: PrefixChain = {

      val list = owners.list.reverse.takeWhile { owner =>
        owner.isStatic
      }.reverse

      PrefixChain(list)

    }

    lazy val outerClass: Option[universe.Symbol] = Option(owners.list.reverse.head).filter { v =>
      v.isClass
    }

//    lazy val packagePrefix: String = packageChain
//      .map { ss =>
//        ss.name.encodedName
//      }
//      .reverse
//      .mkString(".")
//
//    def packagePrefixDot: String = packagePrefix + "."

    override def toString: String = self.fullName
  }
}
