package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.IDMixin

import scala.language.implicitConversions

trait TypeViews extends SymbolViews {

  case class TypeID(
      self: universe.Type
  ) extends IDMixin {

    lazy val symbols: Seq[universe.Symbol] = Seq(
      self.typeSymbol,
      self.termSymbol
    ).filter { ss =>
      ss != getUniverse.NoSymbol
    }

    lazy val showStr: String = self.toString

    override protected def _id: Any = symbols -> showStr
  }

  case class TypeView(
      self: Type,
      comment: Option[String] = None
  ) {

    import com.tribbloids.graph.commons.util.reflect.TypeViews._

    lazy val id: TypeID = TypeID(dealias)

    // TODO: useless?
//    lazy val internal: Option[internalUniverse.Type] = {
//      self match {
//        case tt: internalUniverse.Type =>
//          Some(tt)
//        case _ =>
//          None
//      }
//    }

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

    lazy val baseTypes: List[TypeView] = {

      val baseClzs = self.baseClasses

      val baseNodes = baseClzs.map { clz =>
        val tt = self.baseType(clz)
        if (tt == getUniverse.NoType) TypeView(tt, Some(clz.toString))
        else TypeView(tt)
      }

      baseNodes
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
        Seq(dealias) ++ aliasOpt
      }

      lazy val both: String = {

        variants
          .map { vv =>
            TypeView(vv).Display(format).base
          }
          .distinct
          .mkString(ALIAS_SPLITTER)
      }

      lazy val full: String = (Seq(both) ++ comment).mkString(" ⁇ ")

    }

    //  override def toString: String = show1Line
  }
}

object TypeViews {

  val ALIAS_SPLITTER = " ≅ "
}
