val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {
    testFixturesApi("com.chuusai:shapeless_${vs.scalaBinaryV}:2.3.9")

    testFixturesApi("org.scalatest:scalatest_${vs.scalaBinaryV}:${vs.scalaTestV}")

    testFixturesApi("io.tryp:splain_${vs.scalaV}:${splainVFallback}")
}