val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {
    api("org.scalameta:ascii-graphs_${vs.scalaBinaryV}:0.1.2")
//    api("org.typelevel:cats-effect_${vs.scalaBinaryV}:3.4.5")

    api("com.chuusai:shapeless_${vs.scalaBinaryV}:2.3.9")

    testFixturesApi("org.scalatest:scalatest_${vs.scalaBinaryV}:${vs.scalaTestV}")

//    testFixturesApi("io.tryp:splain_${vs.scalaV}:${splainVFallback}")
}