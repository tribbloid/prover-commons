package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.IDMixin
import com.tribbloids.graph.commons.util.reflect.format.TypeFormat

import scala.language.{implicitConversions, reflectiveCalls}
import scala.tools.reflect.ToolBox
import scala.util.Try

trait TypeViews extends HasUniverse {
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
  ) extends ApiView[Type] {

    final val refl: TypeViews.this.type = TypeViews.this

    lazy val id: TypeID = TypeID(deAlias)

    lazy val constructor: TypeView = TypeView(self.typeConstructor)

    lazy val symbols: Seq[SymbolView] = id.symbols.map(v => SymbolView(v))

    override def getCanonicalName(v: universe.Type): String = v.toString

    lazy val singletonSymbol: Option[universe.Symbol] = {

      (self.termSymbol, self.typeSymbol) match {
        case (termS, _) if termS.isTerm && termS.isStatic => Some(termS)
        case (_, typeS) if typeS.isModuleClass => Some(typeS)
        case _ =>
          None
      }
    }

    def getOnlyInstance: Any = {

      self.dealias match {
        case v: universe.ConstantType =>
          v.value.value
        case v @ _ =>
          val onlySym = TypeView(v).singletonSymbol.getOrElse {
            throw new UnsupportedOperationException(
              s"$v : ${v.getClass} is not a Singleton"
            )
          }

          val mirror = Reflection.Runtime.mirror

          val tool = ToolBox(mirror).mkToolBox()
          val path = onlySym.fullName

          try {
            val result = tool.eval(tool.parse(path))

            if (result == null) {
              throw new UnsupportedOperationException(
                s"$path : ${onlySym.getClass} is not initialised yet"
              )
            }

            result

          } catch {
            case e: Throwable =>
              throw new UnsupportedOperationException(
                s"cannot parse or evaluate $path : ${onlySym.getClass}" +
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

    lazy val prefixOpt: Option[TypeView] = {

      self match {
        case v: Type { def pre: Type } =>
          val pre = Try(TypeView(v.pre)).filter { v =>
            val self = v.self
            val notNone = self != universe.NoPrefix
            //            val notSingle = !self.isInstanceOf[universe.SingleType]
            notNone
          }.toOption

          val result = pre.filter { pre =>
            Prefixes.getCanonicalName(v).startsWith(Prefixes.getCanonicalName(pre.self))
          }
          result

        case _ =>
          None
      }
    }

    object Prefixes extends Breadcrumbs {

      override def getCanonicalName(v: Type): String = {

        val vv = TypeView(v)
        val result = if (vv.singletonSymbol.isDefined) {
          v.toString.stripSuffix(".type")
        } else {
          v.toString
        }

        result
      }

      lazy val internal: BreadcrumbView = {

        val chain = prefixOpt.toList.flatMap { tt =>
          val v1 = List(tt.self)
          val v2 = tt.Prefixes.all.list

          val result = v1 ++ v2
          result
        }

        BreadcrumbView(chain)
      }

      lazy val all: BreadcrumbView = {

        val list = internal.list.filter { tt =>
          tt.toString != "<root>"
        }

        val result = BreadcrumbView(list)
//        result.validate()
        result
      }

      lazy val static: BreadcrumbView = {

        val list = all.list.reverse.takeWhile { tt =>
          TypeView(tt).singletonSymbol.exists { ss =>
            ss.isStatic
          }
        }.reverse

        BreadcrumbView(list)
      }

      lazy val packages: BreadcrumbView = {

        val list = all.list.reverse.takeWhile { tt =>
          TypeView(tt).singletonSymbol.exists { ss =>
            ss.isPackage
          }
        }.reverse

        BreadcrumbView(list)
      }
    }

    lazy val parts: List[TypeView] = {

      val results = prefixOpt.toList.flatMap(_.parts) ++ args

      results.filter { v =>
        self.toString.contains(v.toString)
      }
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
      val result = Formatting(this, format)
      result.text
      result
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
