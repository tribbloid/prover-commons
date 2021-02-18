package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.ScalaReflection.{internalUniverse, universe, Type}

import scala.language.implicitConversions

case class TypeView(
    self: Type,
    comment: Option[String] = None
) {

  import com.tribbloids.graph.commons.util.reflect.TypeView._

  lazy val id: TypeID = TypeID(dealias)

  lazy val internal: Option[internalUniverse.Type] = {
    self match {
      case tt: internalUniverse.Type =>
        Some(tt)
      case _ =>
        None
    }
  }

  lazy val dealias: universe.Type = self.dealias
  lazy val aliasOpt: Option[Type] = Option(self).filterNot(v => v == dealias)

  object Recursive {

    lazy val collectArgs: List[universe.Type] = {

      val selfArgs = self.typeArgs
      val loopEliminated = selfArgs.filterNot(v => v =:= self)

      val transitive: List[universe.Type] = loopEliminated.flatMap { v =>
        TypeView(v).Recursive.collectArgs
      }

      val result = selfArgs ++ transitive

      result
    }

    lazy val collectSymbols: List[universe.Symbol] = (List(self) ++ collectArgs).flatMap(v => TypeView(v).id.symbols)
  }

  case class Display(format: TypeFormat) {

    lazy val base: String = {

      var result: String = self.toString

      if (format.hidePackages) {

        for (ss <- Recursive.collectSymbols) {

          result = result.replaceAllLiterally(SymbolView(ss).packagePrefix, "")
        }
      }

      result
    }

    lazy val variants: Seq[universe.Type] = if (format.hideAlias) {
      Seq(dealias)
    } else {
      (Seq(dealias) ++ aliasOpt)
    }

    lazy val both: String = {

      variants
        .map { vv =>
          (TypeView(vv).Display(format).base)
        }
        .distinct
        .mkString(ALIAS_SPLITTER)
    }

    lazy val full: String = (Seq(both) ++ comment).mkString(" ⁇ ")

  }

//  override def toString: String = show1Line
}

object TypeView {

  val ALIAS_SPLITTER = " \uD83D\uDD37 "
}
