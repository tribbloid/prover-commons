val vs: Versions = versions()

dependencies {

    api(project(":graph-commons"))

    api("org.scalatest:scalatest_${vs.scalaBinaryV}:3.0.8")

    implementation("org.slf4j:slf4j-api:1.7.30")

//    api("eu.timepit:refined_${vv.scalaBinaryV}:0.9.14")
//    /TODO: remove, most arity inspection macros doesn't work on collection/tuple, using shapeless Length as cheap alternative
}