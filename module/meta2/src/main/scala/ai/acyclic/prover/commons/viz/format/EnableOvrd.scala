package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.refl.Reflection
import ai.acyclic.prover.commons.viz.TypeIROutput

import scala.collection.mutable

case class EnableOvrd(
    base: TypeFormat
) extends Formats1.HasBase {

  final def resolve(refl: Reflection): refl.TypeView => TypeIROutput = { tt =>
    val u = refl.getUniverse

    val companions_args = {

      val complete = tt.baseTypes

      complete.to(LazyList).map { v =>
        v.companion -> v.args
      }
    }

    val qualified = companions_args
      .filter { v =>
        val isFormat = v._1 <:< u.typeOf[TypeFormat]
        val isFormatConstructor = v._1 <:< u.typeOf[TypeFormat.Constructor]

        if (isFormat || isFormatConstructor) {
          if (v._2.isEmpty) {
            throw new IllegalArgumentException(s"not applicable to type with TypeFormat ${v._1} and no argument")
          }
          true
        } else {
          false
        }
      }

    qualified
      .flatMap {

        case (companionType, argTypes) =>
          val cached = {
            val cName = companionType.typeSymbol.fullName

            EnableOvrd.cache.getOrElseUpdate(
              cName,
              refl.typeView(companionType).onlyInstance
            )
          }

          val companion: TypeFormat = {
            cached match {
              case v: TypeFormat             => v
              case v: TypeFormat.Constructor => v.apply(this)
              case _ =>
                throw new UnsupportedOperationException(
                  s"expecting TypeFormat or TypeFormat.Constructor, get ${cached.getClass.getCanonicalName}"
                )
            }
          }

          try {
            val outputs = argTypes.map { arg =>
              companion.resolve(refl).apply(arg)
            }

            val textParts = outputs
              .map { v =>
                v.text
              }

            val text = companion.joinText(textParts)

            Some(text: TypeIROutput)
          } catch {
            case _: Backtracking =>
              None
          }
      }
      .headOption
      .getOrElse {

        val byBase = tt.formattedBy(base)
        byBase.text <:^ Seq(byBase)
      }
  }
}

object EnableOvrd extends EnableOvrd(TypeFormat.Default) {

  val cache: mutable.HashMap[String, Any] = {

    mutable.HashMap.empty
  }
}
