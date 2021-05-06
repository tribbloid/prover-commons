package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.IDMixin
import com.tribbloids.graph.commons.util.reflect.format.TypeFormat

import scala.language.implicitConversions
import scala.tools.reflect.ToolBox
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

    lazy val constructor: TypeView = TypeView(self.typeConstructor)

    lazy val symbols = id.symbols.map(v => SymbolView(v))

    lazy val fullString: String = self.toString

    lazy val shortString: String = {

      var str = self.toString
      for (ss <- symbols) {
        str = str.stripPrefix(ss.packagePrefixDot)
      }
      str
    }

    def getOnlyInstance: Any = {

      self.dealias match {
        case v: universe.ConstantType =>
          v.value.value
        case v @ _ =>
          val onlySym = (v.termSymbol, v.typeSymbol) match {
            case (term, _) if term.isTerm && term.isStatic => term
            case (_, tt) if tt.isModuleClass => tt
            case _ =>
              throw new UnsupportedOperationException(
                s"${v} : ${v.getClass} is not a Singleton"
              )
          }

          val mirror = Reflection.Runtime.mirror

          val tb = ToolBox(mirror).mkToolBox()
          val path = onlySym.fullName

          try {
            tb.eval(tb.parse(path))
          } catch {
            case e: Throwable =>
              throw new UnsupportedOperationException(
                s"cannot parse or evaluate ${path} : ${onlySym.getClass}" +
                  "\n\t" + e.toString,
                e
              )
          }
      }
    }

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

    lazy val args: List[TypeView] = self.typeArgs.map { arg =>
      TypeView(arg)
    }

    lazy val baseTypes: List[TypeView] = {

      val baseClzSyms = self.baseClasses

      val baseNodes = self match {
        case v: Type with scala.reflect.internal.Types#Type =>
          val list = v.baseTypeSeq.toList.map { v =>
            v.asInstanceOf[Type] //https://github.com/scala/bug/issues/9837
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

    object Recursive {

      lazy val collectArgs: Seq[TypeView] = {

        val selfArgs = self.typeArgs
        val loopEliminated = selfArgs.filterNot(v => v =:= self)

        val transitive = loopEliminated.flatMap { v =>
          TypeView(v).Recursive.collectArgs
        }

        val result = args ++ transitive

        result
      }

      lazy val collectSymbols: List[SymbolView] =
        (List(TypeView.this) ++ collectArgs).flatMap(v => v.id.symbols).map { v =>
          SymbolView(v)
        }
    }
  }

}

object TypeViews {

  val ALIAS_SPLITTER = " ≅ "
}
