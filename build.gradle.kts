val vs: Versions = versions()

dependencies {


//    api("eu.timepit:refined_${vv.scalaBinaryV}:0.9.14")
//    /TODO: remove, most arity inspection macros doesn't work on collection/tuple, using shapeless Length as cheap alternative

//    api("com.chuusai:shapeless_${vs.scalaBinaryV}:2.3.3")

    api("com.lihaoyi:pprint_${vs.scalaBinaryV}:0.5.6")
}