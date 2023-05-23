//val versions = gradle.rootProject.versions()


include("prover-commons")
project(":prover-commons").projectDir = file("module")

include(
    ":prover-commons:core",
    ":prover-commons:meta",
    ":notebook"
)


pluginManagement.repositories {
    gradlePluginPortal()
    mavenCentral()
    // maven("https://dl.bintray.com/kotlin/kotlin-dev")
}
