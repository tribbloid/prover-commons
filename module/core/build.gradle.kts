val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {
    api("org.scalameta:ascii-graphs_${vs.scala.binaryV}:0.1.2")
//    api("org.typelevel:cats-effect_${vs.scala.binaryV}:3.4.5")

    api("com.chuusai:shapeless_${vs.scala.binaryV}:2.3.9")

    testFixturesApi("org.scalatest:scalatest_${vs.scala.binaryV}:${vs.scalaTestV}")

    testFixturesApi("io.tryp:splain_${vs.scala.v}:${splainVFallback}")
}