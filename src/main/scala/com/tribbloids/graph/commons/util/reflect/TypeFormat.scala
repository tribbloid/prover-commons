package com.tribbloids.graph.commons.util.reflect

// TODO: this should be broken into type format and tree format
case class TypeFormat(
    hidePackages: Boolean = false,
    nameOf: TypeFormat.nameOf.Value = TypeFormat.nameOf.Type,
    variants: TypeFormat.variants.Value = TypeFormat.variants.Both
) {}

object TypeFormat {

  object nameOf extends Enumeration {

    val Type, TypeConstructor, Class = Value
  }

  object variants extends Enumeration {

    val Alias, Dealias, Both = Value
  }

  object Default extends TypeFormat()
}
