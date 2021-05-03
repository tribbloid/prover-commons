package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.format

import scala.collection.mutable

case class EnableOvrd(
    lastResort: TypeFormat
) extends TypeFormat {

  override def resolve(ff: Formatting): Output = {

    val refl = ff.refl
    val u = refl.getUniverse
    val tt = ff.typeView.self.asInstanceOf[u.Type]

//    lazy val typeInfoName = u.weakTypeOf[HasTypeInfo#_TypeInfo].typeSymbol.name

//    val infoTT: u.Type = {
//      if (tt <:< u.weakTypeOf[HasTypeInfo]) {
//        tt.member(typeInfoName).typeSignatureIn(tt)
//      } else {
//        tt
//      }
//    }

    val infoTT = tt

    //    val companionTs = Stream(infoT.companion)

    val companions_args = {

      val complete = refl
        .TypeView(infoTT)
        .baseTypes
        .map { v =>
          v.self
        }

      complete.to(LazyList).map { v =>
        v.companion -> v.typeArgs
      }
    }

    val qualified = companions_args
      .filter { v =>
        val hasFormat = v._1 <:< u.typeOf[format.TypeFormat]

        if (hasFormat && v._2.isEmpty) {
          throw new IllegalArgumentException(s"not applicable to type with TypeFormat ${v._1} and no argument")
        }
        hasFormat
      }

    qualified
      .flatMap { v =>
        val companion = {

          val cName = v._1.typeSymbol.fullName

//          print_@(v._1.typeSymbol.fullName)
//          print_@(v._1.termSymbol.fullName)

          EnableOvrd.cache.getOrElseUpdate(
            cName,
            refl.TypeView(v._1).getOnlyInstance.asInstanceOf[TypeFormat]
          )

        }

        try {
          val outputs = v._2.map { arg =>
            val _ff = refl.Formatting(
              refl.TypeView(arg),
              ff.format
            )

            companion.resolve(_ff)
          }

          val textParts = outputs
            .map { v =>
              v.text
            }

          val text = companion.joinText(textParts)

          Some(text -> outputs.flatMap(v => v.causes): Output)
        } catch {
          case e: UnsupportedOperationException =>
            None
        }
      }
      .headOption
      .getOrElse {
        ff.formattedBy(lastResort)
      }
  }
}

object EnableOvrd extends EnableOvrd(TypeFormat.Default) {

  val cache: mutable.HashMap[String, TypeFormat] = {

//    val list = Seq(Singleton, ~~).map { v =>
//      v.getClass.getCanonicalName.stripSuffix("$") -> v
//    }
//
//    mutable.HashMap(
//      list: _*
//    )

    mutable.HashMap.empty
  }
}