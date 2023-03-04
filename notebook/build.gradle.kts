val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {
    api(project(":prover-commons:core"))

    testFixturesApi(testFixtures(project(":prover-commons:core")))
}