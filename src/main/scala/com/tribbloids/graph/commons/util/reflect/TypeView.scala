package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.ScalaReflection.{internalUniverse, universe, Type}

import scala.language.implicitConversions

case class TypeView(
    self: Type,
    comment: Option[String] = None,
) {

  import com.tribbloids.graph.commons.util.reflect.TypeView._

  lazy val id: TypeID = TypeID(dealiased)

  lazy val internal: Option[internalUniverse.Type] = {
    self match {
      case tt: internalUniverse.Type =>
        Some(tt)
      case _ =>
        None
    }
  }

  lazy val dealiased: universe.Type = self.dealias
  lazy val variants: Seq[universe.Type] = Seq(
    self,
    dealiased
  ).distinct

  lazy val symbols: Seq[universe.Symbol] = Seq(
    self.typeSymbol,
    self.termSymbol
  ).filter { ss =>
    ss != universe.NoSymbol
  }

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

    lazy val collectSymbols: List[universe.Symbol] = (List(self) ++ collectArgs).flatMap(v => TypeView(v).symbols)
  }

  case class Viz(format: TypeFormat) {

    lazy val base: String = {

      var result: String = self.toString

      if (format.hidePackages) {

        for (ss <- Recursive.collectSymbols) {

          result = result.replaceAllLiterally(SymbolView(ss).packagePrefix, "")
        }
      }

      result
    }

    lazy val deduction: String = {

      variants
        .map { vv =>
          (TypeView(vv).Viz(format).base)
        }
        .distinct
        .mkString(DEALIAS)
    }

    lazy val full: String = (Seq(deduction) ++ comment).mkString(" ⁇ ")

  }

//  override def toString: String = show1Line
}

object TypeView {

  val DEALIAS = " ==\uD83D\uDD37=>  "
}
