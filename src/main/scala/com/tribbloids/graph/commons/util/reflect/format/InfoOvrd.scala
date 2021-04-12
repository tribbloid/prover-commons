package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.format.TypeFormat.Output
import com.tribbloids.graph.commons.util.reflect.{format, Reflection}

import scala.util.Try

case class InfoOvrd(
    fallback: TypeFormat
) extends TypeFormat {

  override def resolve(ff: Formatting): Output = {

    val refl = ff.refl
    val u = refl.universe

    val tt = ff.typeView.self.asInstanceOf[u.Type]

    lazy val typeInfoName = u.weakTypeOf[HasTypeInfo#_TypeInfo].typeSymbol.name

    val infoTT: u.Type = {
      if (tt <:< u.typeOf[HasTypeInfo]) {
        tt.member(typeInfoName).typeSignatureIn(tt)
      } else {
        tt
      }
    }

    //    val companionTs = Stream(infoT.companion)

    val companionTs = {

      val complete = refl
        .TypeView(infoTT)
        .baseTypes
        .map { v =>
          v.self
        }

      complete.toStream.map(v => v.companion)
    }

    companionTs
      .filter(v => v <:< u.typeOf[format.TypeFormat])
      .flatMap { v =>
        val companion = {

          // cross universe operation may be unsafe?
          val mR = Reflection.Runtime.mirror
          val companionName = v.typeSymbol.fullName

          val companionR = mR.reflectModule(mR.staticModule(companionName))
          companionR.instance.asInstanceOf[TypeFormat]
        }

        Try {
          val outputs = infoTT.typeArgs.map { arg =>
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

          text -> outputs.flatMap(v => v.children): Output
        }.toOption
      }
      .headOption // at the moment, _Info are mutually exclusive
      .getOrElse {
        ff.formattedBy(fallback)
      }
  }
}

object InfoOvrd extends InfoOvrd(TypeFormat.Default) {}
