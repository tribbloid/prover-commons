//val versions = gradle.rootProject.versions()


include("prover-commons")
project(":prover-commons").projectDir = file("module")

include(
    ":prover-commons:infra",
    ":prover-commons:core",
    ":prover-commons:meta2", // should only be working for Scala 2
    ":prover-commons:spark",
    ":prover-commons:abandoned",
    ":notebook"
)


pluginManagement.repositories {
    gradlePluginPortal()
    mavenCentral()
    // maven("https://dl.bintray.com/kotlin/kotlin-dev")
}
