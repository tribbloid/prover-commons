val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {
//    api("org.typelevel:cats-effect_${vs.scalaBinaryV}:3.4.5")

    testFixturesApi("com.chuusai:shapeless_${vs.scalaBinaryV}:2.3.9")

    testFixturesApi("org.scalatest:scalatest_${vs.scalaBinaryV}:${vs.scalaTestV}")

    testFixturesApi("io.tryp:splain_${vs.scalaV}:${splainVFallback}")
}