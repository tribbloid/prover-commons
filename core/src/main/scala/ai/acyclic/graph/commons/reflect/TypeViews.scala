package ai.acyclic.graph.commons.reflect

import ai.acyclic.graph.commons.EqualBy
import ai.acyclic.graph.commons.reflect.format.TypeFormat

import scala.collection.mutable
import scala.tools.reflect.ToolBox
import scala.util.Try

trait TypeViews extends HasUniverse {
  self: Reflection =>

  case class TypeID(
      self: Type
  ) extends EqualBy {

    lazy val allSymbols: Seq[Symbol] = {

      val sym = self match {
        case v: universe.ThisTypeApi   => Some(v.sym)
        case v: universe.SingleTypeApi => Some(v.sym)
        case v: universe.TypeRefApi    => Some(v.sym)
        case _                         => None
      }

      val proto = sym.toSeq ++ Seq(
        self.typeSymbol,
        self.termSymbol
      )

      proto.filter { ss =>
        ss != universe.NoSymbol
      }.distinct
    }

    lazy val showStr: String = self.toString

    override protected def _equalBy: Any = allSymbols -> showStr
  }

  case class TypeView(
      self: Type
//      comment: Option[String] = None // TODO: useless?
  ) extends ApiView[Type] {

    final val refl: TypeViews.this.type = TypeViews.this

    lazy val _deAlias: Type = self.dealias

    lazy val id: TypeID = {
      TypeID(self)
    }

    lazy val reference: TypeID = {
      TypeID(_deAlias)
    }

    lazy val constructor: TypeView = typeView(self.typeConstructor)

    lazy val allSymbols: Seq[SymbolView] = id.allSymbols.map(v => symbolView(v))

    override lazy val canonicalName: String = self.toString

    lazy val singletonSymbol: Option[Symbol] = {

      (self.termSymbol, self.typeSymbol) match {
        case (termS, _) if termS.isTerm && termS.isStatic => Some(termS)
        case (_, typeS) if typeS.isModuleClass            => Some(typeS)
        case _ =>
          None
      }
    }

    lazy val singletonName: String = {

      _deAlias match {
        case universe.ConstantType(v) =>
          "" + v.value
        case v @ _ =>
          val onlySym = typeView(v).singletonSymbol.getOrElse {
            throw new UnsupportedOperationException(
              s"$v : ${v.getClass} is not a Singleton"
            )
          }

          onlySym.fullName
      }
    }

    // TODO: this should be removed as the only instance may not be exposed at compile time
    //  use singletonValue if possible
    lazy val onlyInstance: Any = {

      _deAlias match {
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

          lazy val pathInfo = s"$path : ${onlySym.getClass}"

          try {
            val result = tool.eval(tool.parse(path))

            if (result == null) {
              throw new UnsupportedOperationException(
                s"$pathInfo is not initialised yet"
              )
            }

            result

          } catch {
            case e: Throwable =>
              throw new UnsupportedOperationException(
                s"cannot evaluate $pathInfo, it may be undefined in this compilation stage" +
                  "\n\t" + e.toString,
                e
              )
          }
      }
    }

    lazy val _aliasOpt: Option[Type] = Option(self).filterNot(v => v == _deAlias)

    lazy val args: List[TypeView] = self.typeArgs.map { arg =>
      typeView(arg)
    }

    lazy val prefixOpt: Option[TypeView] = {

      import scala.language.reflectiveCalls

      constructor.self match {

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
            v.asInstanceOf[Type] // https://github.com/scala/bug/issues/9837
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
        (List(TypeView.this) ++ collectArgs).flatMap(v => v.id.allSymbols).map { v =>
          symbolView(v)
        }
    }

    override def _copy(self: Type) = copy(self)
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
