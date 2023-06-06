val vs = versions()

buildscript {
    repositories {
        // Add here whatever repositories you're already using
        mavenCentral()
    }
}

plugins {
    java
    `java-library`
    `java-test-fixtures`

    scala
    id("io.github.cosmicsilence.scalafix") version "0.1.14"

    idea

//    signing
//    `maven-publish`
//    id("io.github.gradle-nexus.publish-plugin") version "1.1.0"

    id("com.github.ben-manes.versions") version "0.44.0"
}

allprojects {

    apply(plugin = "java")
    apply(plugin = "java-library")
    apply(plugin = "java-test-fixtures")

    // apply(plugin = "bloop")
    // DO NOT enable! In VSCode it will cause the conflict:
    // Cannot add extension with name 'bloop', as there is an extension already registered with that name

    apply(plugin = "scala")
    apply(plugin = "io.github.cosmicsilence.scalafix")

    apply(plugin = "idea")

    apply(plugin = "signing")
    apply(plugin = "maven-publish")

    version = vs.projectV

    repositories {
        mavenLocal()
        mavenCentral()
        maven("https://dl.bintray.com/kotlin/kotlin-dev")
        maven("https://scala-ci.typesafe.com/artifactory/scala-integration/") // scala SNAPSHOT
    }

    dependencies {

        // see https://github.com/gradle/gradle/issues/13067
//        fun both(constraintNotation: Any) {
//            implementation(constraintNotation)
//            testFixturesImplementation(constraintNotation)
//        }

        implementation("${vs.scala.group}:scala-library:${vs.scala.v}")

        testFixturesApi("org.scalatest:scalatest_${vs.scala.binaryV}:${vs.scalaTestV}")
        testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")

        testRuntimeOnly("co.helmethair:scalatest-junit-runner:0.2.0")
    }

    task("dependencyTree") {

        dependsOn("dependencies")
    }

    tasks {

        withType<ScalaCompile> {

            scalaCompileOptions.apply {

//                    isForce = true

                loggingLevel = "verbose"

                val compilerOptions =

                    mutableListOf(
                        "-encoding", "UTF-8",
                        "-unchecked", "-deprecation", "-feature",

                        "-language:higherKinds",

                        "-Xlint:poly-implicit-overload", "-Xlint:option-implicit", "-Wunused:imports",

                        "-g:vars",

                        )

                additionalParameters = compilerOptions

                forkOptions.apply {

                    memoryInitialSize = "1g"
                    memoryMaximumSize = "4g"

                    // this may be over the top but the test code in macro & core frequently run implicit search on church encoded Nat type
                    jvmArgs = listOf(
                        "-Xss256m"
                    )
                }
            }
        }

        test {

            minHeapSize = "1024m"
            maxHeapSize = "4096m"

            testLogging {
                showExceptions = true
                showCauses = true
                showStackTraces = true

                // stdout is used for occasional manual verification
                showStandardStreams = true
            }

            useJUnitPlatform {
                includeEngines("scalatest")
                testLogging {
                    events("passed", "skipped", "failed")
                }
            }
        }
    }

    java {
        withSourcesJar()
        withJavadocJar()
    }


    idea {

        module {

            excludeDirs = excludeDirs + files(

                "target",
                "out",

                ".idea",
                ".vscode",
                ".bloop",
                ".bsp",
                ".metals",
                "bin",

                ".ammonite",

                "logs",

                )

            isDownloadJavadoc = true
            isDownloadSources = true
        }
    }
}

//subprojects {
//
//}

idea {

    targetVersion = "2020"

    module {

        excludeDirs = excludeDirs + listOf(
            file(".gradle"),
            file("gradle")
        )
    }
}
