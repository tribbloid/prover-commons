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
        fun both(notation: Any) {
            implementation(notation)
            testFixturesImplementation(notation)
        }

        both("${vs.scala.group}:scala-library:${vs.scala.v}")

        //https://github.com/tek/splain
        if (vs.splainV.isNotEmpty()) {
            val splainD = "io.tryp:splain_${vs.scala.v}:${vs.splainV}"
            logger.warn("${project.displayName} / scalaCompilerPlugins:\n\t --- using ${splainD}")

            scalaCompilerPlugins(splainD)
        }
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
//                        "-Xsource:3",

                        "-Xlint:poly-implicit-overload", "-Xlint:option-implicit", "-Wunused:imports",

                        "-g:vars",

                        )

                if (vs.splainV.isNotEmpty()) {
                    compilerOptions.addAll(
                        listOf(
                            "-Vimplicits", "-Vimplicits-verbose-tree", "-Vtype-diffs",
                            "-P:splain:Vimplicits-diverging",
                            "-P:splain:Vtype-reduction",
                            "-P:splain:Vtype-detail:3",
                            "-P:splain:Vtype-diffs-detail:3",
                            "-P:splain:Vdebug"
                        )
                    )
                }

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

                ".cache",
                ".history",
                ".lib",

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
