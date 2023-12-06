val vs = versions()

val splainVRelease: String = "1.1.0-RC0"

val splainVFallback: String = run {

    vs.splainV.ifEmpty { splainVRelease }
}

val splainD: String = run {
    val splainD = "io.tryp:splain_${vs.scala.v}:${splainVFallback}"
    logger.warn("using ${splainD} ...\t in ${project.displayName} / dependencies")
    splainD
}

dependencies {
    api("org.scalameta:ascii-graphs_${vs.scala.binaryV}:0.1.2")
//    api("org.typelevel:cats-effect_${vs.scala.binaryV}:3.4.5")

    api("com.chuusai:shapeless_${vs.scala.binaryV}:2.3.10")

    testFixturesApi(splainD)
}