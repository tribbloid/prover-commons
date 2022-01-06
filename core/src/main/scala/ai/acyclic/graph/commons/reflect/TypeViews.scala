package ai.acyclic.graph.commons.reflect

import ai.acyclic.graph.commons.IDMixin
import ai.acyclic.graph.commons.reflect.format.TypeFormat

import scala.collection.mutable
import scala.tools.reflect.ToolBox
import scala.util.Try

trait TypeViews extends HasUniverse {
  self: Reflection =>

  case class TypeID(
      self: Type
  ) extends IDMixin {

    lazy val symbols: Seq[Symbol] = Seq(
      self.typeSymbol,
      self.termSymbol
    ).filter { ss =>
      ss != universe.NoSymbol
    }

    lazy val showStr: String = self.toString

    override protected def _id: Any = symbols -> showStr
  }

  case class TypeView(
      self: Type
//      comment: Option[String] = None // TODO: useless?
  ) extends ApiView[Type] {

    final val refl: TypeViews.this.type = TypeViews.this

    lazy val id: TypeID = TypeID(deAlias)

    lazy val constructor: TypeView = typeView(self.typeConstructor)

    lazy val symbols: Seq[SymbolView] = id.symbols.map(v => symbolView(v))

    override def getCanonicalName(v: Type): String = v.toString

    lazy val singletonSymbol: Option[Symbol] = {

      (self.termSymbol, self.typeSymbol) match {
        case (termS, _) if termS.isTerm && termS.isStatic => Some(termS)
        case (_, typeS) if typeS.isModuleClass => Some(typeS)
        case _ =>
          None
      }
    }

    lazy val getOnlyInstance: Any = {

      // TODO: add mnemonic
      self.dealias match {
        case universe.ConstantType(v) =>
          v.value
        case v @ _ =>
          val onlySym = typeView(v).singletonSymbol.getOrElse {
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
    //    lazy val internal: Option[internalType] = {
    //      self match {
    //        case tt: internalType =>
    //          Some(tt)
    //        case _ =>
    //          None
    //      }
    //    }

    lazy val deAlias: Type = self.dealias
    lazy val aliasOpt: Option[Type] = Option(self).filterNot(v => v == deAlias)

    lazy val args: List[TypeView] = self.typeArgs.map { arg =>
      typeView(arg)
    }

    lazy val prefixOpt: Option[TypeView] = {

      import scala.language.reflectiveCalls

      self match {
        case v: Type { def pre: Type } =>
          val pre = Try(typeView(v.pre)).filter { v =>
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

        val vv = typeView(v)
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
          typeView(tt).singletonSymbol.exists { ss =>
            ss.isStatic
          }
        }.reverse

        BreadcrumbView(list)
      }

      lazy val packages: BreadcrumbView = {

        val list = all.list.reverse.takeWhile { tt =>
          typeView(tt).singletonSymbol.exists { ss =>
            ss.isPackage
          }
        }.reverse

        BreadcrumbView(list)
      }
    }

    // generalised arguments, also include arguments of prefixes
    lazy val genArgs: List[TypeView] = {

      val results = prefixOpt.toList.flatMap(_.genArgs) ++ args

      results.filter { v =>
        self.toString.contains(v.toString)
      } // TODO: this should be moved to elsewhere
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
            typeView(tt)
          }
        case _ =>
          baseClzSyms.map { clz =>
            val tt = self.baseType(clz)
            if (tt == universe.NoType) typeView(tt)
            else typeView(tt)
          }

      }

      baseNodes
    }

    def formattedBy(format: TypeFormat): TypeIR = {
      val result = TypeIR(this, format)
      result.text
      result
    }

    //  override def toString: String = show1Line

    object Recursive {

      lazy val collectArgs: Seq[TypeView] = {

        val loopEliminated = args.filterNot(v => v.self =:= self)

        val transitive = loopEliminated.flatMap { v =>
          v.Recursive.collectArgs
        }

        val result = args ++ transitive

        result
      }

      lazy val collectSymbols: List[SymbolView] =
        (List(TypeView.this) ++ collectArgs).flatMap(v => v.id.symbols).map { v =>
          symbolView(v)
        }
    }
  }

  val typeCache = mutable.Map.empty[Type, TypeView]

  def typeView(tt: Type): TypeView = typeCache.getOrElseUpdate(
    tt,
    TypeView(tt)
  )
}

object TypeViews {

  val ALIAS_SPLITTER = " ≅ "
}
