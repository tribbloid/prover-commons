//val versions = gradle.rootProject.versions()


include("prover-commons")
project(":prover-commons").projectDir = file("module")

include(
    ":prover-commons:infra",
    ":prover-commons:core",
    ":prover-commons:meta2", // should only be working for Scala 2
//    ":prover-commons:record",
    ":prover-commons:spark",
    ":prover-commons:abandoned",
    ":notebook"
)


pluginManagement {
    resolutionStrategy {
        eachPlugin {
            if (requested.id.id.startsWith("ai.acyclic")) {
                useModule("ai.acyclic:buildSrc:1.0-SNAPSHOT")
            }
        }
    }
    repositories {
        gradlePluginPortal()
        mavenCentral()
        // maven("https://dl.bintray.com/kotlin/kotlin-dev")
    }
}

includeBuild("../buildSrc") {
    name = "shared-build-logic"
    dependencySubstitution {
        substitute(module("ai.acyclic:buildSrc")).using(project(":"))
    }
}
