//val versions = gradle.rootProject.versions()


include(
    ":core",
    ":meta",
    ":notebook"
)


pluginManagement.repositories {
    gradlePluginPortal()
    mavenCentral()
    // maven("https://dl.bintray.com/kotlin/kotlin-dev")
}
