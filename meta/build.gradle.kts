val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {

    implementation("${vs.scalaGroup}:scala-compiler:${vs.scalaV}")
    implementation("${vs.scalaGroup}:scala-reflect:${vs.scalaV}")

    api(project(":core"))
    testFixturesApi(testFixtures(project(":core")))
}