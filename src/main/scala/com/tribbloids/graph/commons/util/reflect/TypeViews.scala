package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.IDMixin
import com.tribbloids.graph.commons.util.reflect.format.TypeFormat

import scala.language.implicitConversions
import scala.util.Try

trait TypeViews extends SymbolViews {
  self: Reflection =>

  case class TypeID(
      self: universe.Type
  ) extends IDMixin {

    lazy val symbols: Seq[universe.Symbol] = Seq(
      self.typeSymbol,
      self.termSymbol
    ).filter { ss =>
      ss != universe.NoSymbol
    }

    lazy val showStr: String = self.toString

    override protected def _id: Any = symbols -> showStr
  }

  case class TypeView(
      self: Type,
      comment: Option[String] = None
  ) {

    final val refl: TypeViews.this.type = TypeViews.this

    lazy val id: TypeID = TypeID(deAlias)

    // TODO: useless?
//    lazy val internal: Option[internalUniverse.Type] = {
//      self match {
//        case tt: internalUniverse.Type =>
//          Some(tt)
//        case _ =>
//          None
//      }
//    }

    lazy val deAlias: universe.Type = self.dealias
    lazy val aliasOpt: Option[Type] = Option(self).filterNot(v => v == deAlias)

    object Recursive {

      lazy val collectArgs: Seq[TypeView] = {

        val selfArgs = self.typeArgs
        val loopEliminated = selfArgs.filterNot(v => v =:= self)

        val transitive = loopEliminated.flatMap { v =>
          TypeView(v).Recursive.collectArgs
        }

        val result = selfArgs.map { v =>
          TypeView(v)
        } ++ transitive

        result
      }

      lazy val collectSymbols: List[SymbolView] =
        (List(TypeView.this) ++ collectArgs).flatMap(v => v.id.symbols).map { v =>
          SymbolView(v)
        }
    }

    lazy val baseTypes: List[TypeView] = {

      val baseClzSyms = self.baseClasses

      val baseNodes = self match {
        case v: universe.Type with scala.reflect.internal.Types#Type =>
          val list = v.baseTypeSeq.toList.map { v =>
            v.asInstanceOf[Type]
          }

          val withIndices = list.map { tt =>
            val index = Try {
              baseClzSyms.indexOf(tt.typeSymbol.asClass)
            }
              .getOrElse(-2)

            tt -> index
          }

          val reAligned = withIndices.sortBy(_._2).map(_._1)

          reAligned.map { tt =>
            TypeView(tt)
          }
        case _ =>
          baseClzSyms.map { clz =>
            val tt = self.baseType(clz)
            if (tt == universe.NoType) TypeView(tt, Some(clz.toString))
            else TypeView(tt)
          }

      }

      baseNodes
    }

    def formattedBy(format: TypeFormat): Formatting = {
      Formatting(this, format)
    }

    //  override def toString: String = show1Line
  }

}

object TypeViews {

  val ALIAS_SPLITTER = " ≅ "
}
