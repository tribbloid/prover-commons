package ai.acyclic.prover.commons.meta

import ai.acyclic.prover.commons.function
import ai.acyclic.prover.commons.function.Impl
import ai.acyclic.prover.commons.same.Same

import scala.tools.reflect.ToolBox
import scala.util.Try

private[meta] trait TypeViewMixin extends HasUniverse {
  self: ITyper =>

  case class TypeID(
      self: Type
  ) extends Same.ByEquality.IWrapper {

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

    override def samenessDelegatedTo: Any = allSymbols -> showStr
  }

  case class TypeView(
      protected val delegate: Type
  ) extends ApiView[Type] {

    TypeViewMixin.this

    private lazy val _deAlias = delegate.dealias
    final def dealias: TypeView = typeView(_deAlias)

    lazy val id: TypeID = {
      TypeID(delegate)
    }

    lazy val reference: TypeID = {
      TypeID(_deAlias)
    }

    def =:=(that: TypeView): Boolean = {
      delegate =:= that.delegate
    }

    def map(fn: TypeView => TypeView): TypeView = TypeView {
      val result = delegate.map { v =>
        fn(typeView(v)).delegate
      }

      result
    }

    lazy val constructor: TypeView = typeView(delegate.typeConstructor)

    lazy val allSymbols: Seq[SymbolView] = id.allSymbols.map(v => symbolView(v))

    override lazy val canonicalName: String = delegate.toString

    lazy val singletonSymbolOpt: Option[Symbol] = {

      (delegate.termSymbol, delegate.typeSymbol) match {
        case (termS, _) if termS.isTerm && termS.isStatic => Some(termS)
        case (_, typeS) if typeS.isModuleClass            => Some(typeS)
        case _ =>
          None
      }
    }

    def singletonSymbol: Symbol = singletonSymbolOpt.getOrElse {
      throw new UnsupportedOperationException(
        s"$delegate : ${delegate.getClass} is not a Singleton"
      )
    }

    lazy val singletonName: String = {

      _deAlias match {
        case v: universe.ConstantType @unchecked =>
          "" + v.value.value
        case v @ _ =>
          val onlySym = typeView(v).singletonSymbol

          onlySym.fullName
      }
    }

    // TODO: this should be removed as the only instance may not be exposed at compile time
    //  use singletonValue if possible
    lazy val onlyInstance: Any = {

      _deAlias match {
        case v: universe.ConstantType @unchecked =>
          v.value.value
        case v @ _ =>
          val onlySym = typeView(v).singletonSymbol

          val mirror = ScalaReflection.mirror

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

    lazy val _aliasOpt: Option[Type] = Option(delegate).filterNot(v => v == _deAlias)

    lazy val args: List[TypeView] = delegate.typeArgs.map { arg =>
      typeView(arg)
    }

    lazy val prefixOpt: Option[TypeView] = {

      import scala.language.reflectiveCalls

      constructor.delegate match {

        case v: (Type { def pre: Type }) @unchecked =>
          val pre = Try(typeView(v.pre)).filter { v =>
            val self = v.delegate
            val notNone = self != universe.NoPrefix
            //            val notSingle = !self.isInstanceOf[universe.SingleType]
            notNone
          }.toOption

          val result = pre.filter { pre =>
            Prefixes.getCanonicalName(v).startsWith(Prefixes.getCanonicalName(pre.delegate))
          }
          result

        case _ =>
          None
      }
    }

    object Prefixes extends Breadcrumbs {

      override def getCanonicalName(v: Type): String = {

        val vv = typeView(v)
        val result = if (vv.singletonSymbolOpt.isDefined) {
          v.toString.stripSuffix(".type")
        } else {
          v.toString
        }

        result
      }

      lazy val internal: BreadcrumbView = {

        val chain = prefixOpt.toList.flatMap { tt =>
          val v1 = List(tt.delegate)
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
          typeView(tt).singletonSymbolOpt.exists { ss =>
            ss.isStatic
          }
        }.reverse

        BreadcrumbView(list)
      }

      lazy val packages: BreadcrumbView = {

        val list = all.list.reverse.takeWhile { tt =>
          typeView(tt).singletonSymbolOpt.exists { ss =>
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
        delegate.toString.contains(v.toString)
      } // TODO: this should be moved to elsewhere
    }

    lazy val baseTypes: List[TypeView] = {

      val baseClzSyms = delegate.baseClasses

      val baseNodes = delegate match {
        case v: (Type with scala.reflect.internal.Types#Type) @unchecked =>
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
            val tt = delegate.baseType(clz)
            if (tt == universe.NoType) typeView(tt)
            else typeView(tt)
          }

      }

      baseNodes
    }

    lazy val superTypes: List[TypeView] = baseTypes
      .filterNot(tt => tt.reference == this.reference)

    lazy val superTypes_transitive: List[TypeView] = {
      superTypes.flatMap { tt =>
        tt.superTypes
      }.distinct
    }

    lazy val superTypes_nonTransitive: List[TypeView] = {
      val transitiveSet = superTypes_transitive.toSet
      superTypes.filterNot { tt =>
        transitiveSet.contains(tt)
      }
    }

    //  override def toString: String = show1Line

    object Recursive {

      lazy val collectArgs: Seq[TypeView] = {

        val loopEliminated = args.filterNot(v => v.delegate =:= delegate)

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

  lazy val typeView: function.System.FnImpl.Cached[Type, TypeView] = {
    Impl(TypeView.apply _).cachedBy(Same.ByEquality.Lookup())
  }

//  val typeCache = mutable.Map.empty[Type, TypeView]
//
//  def typeView(tt: Type): TypeView = typeCache.getOrElseUpdate(
//    tt,
//    TypeView(tt)
//  )
}

object TypeViewMixin {

  val ALIAS_SPLITTER = " ≅ "
}
