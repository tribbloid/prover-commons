val vs = versions()

buildscript {
    repositories {
        // Add here whatever repositories you're already using
        mavenCentral()
    }
}

plugins {
    java
    `java-test-fixtures`

    scala

    idea

//    signing
//    `maven-publish`
//    id("io.github.gradle-nexus.publish-plugin") version "1.1.0"

    id("com.github.ben-manes.versions") version "0.44.0"
}

group = vs.projectGroup
version = vs.projectV

allprojects {

    apply(plugin = "java")
    apply(plugin = "java-library")
    apply(plugin = "java-test-fixtures")

    // apply(plugin = "bloop")
    // DO NOT enable! In VSCode it will cause the conflict:
    // Cannot add extension with name 'bloop', as there is an extension already registered with that name

    apply(plugin = "scala")
    apply(plugin = "idea")

    apply(plugin = "signing")
    apply(plugin = "maven-publish")

    repositories {
        mavenLocal()
        mavenCentral()
        maven("https://dl.bintray.com/kotlin/kotlin-dev")
        maven("https://scala-ci.typesafe.com/artifactory/scala-integration/") // scala SNAPSHOT
    }

    dependencies {

        // see https://github.com/gradle/gradle/issues/13067
        fun both(constraintNotation: Any) {
            implementation(constraintNotation)
            testFixturesImplementation(constraintNotation)
        }

        constraints {

            both("${vs.scalaGroup}:scala-compiler:${vs.scalaV}")
            both("${vs.scalaGroup}:scala-library:${vs.scalaV}")
        }

        val scalaTestV = "3.2.3"
        testImplementation("org.scalatest:scalatest_${vs.scalaBinaryV}:${scalaTestV}")
        testImplementation("org.junit.jupiter:junit-jupiter:5.9.1")

        testRuntimeOnly("co.helmethair:scalatest-junit-runner:0.2.0")
    }

    //TODO: find more comprehensive solution
//    sourceSets {
//        main {
//            scala {
//                val vn = VersionNumber.parse(vs.scalaV)
//
//                val supportedPatchVs = listOf(6, 7)
//
//                for (from in supportedPatchVs) {
//                    if (vn.micro >= from)
//                        setSrcDirs(srcDirs + listOf("src/main/scala-2.13.${from}+/latest"))
//                    for (to in supportedPatchVs) {
//                        if (vn.micro <= to)
//                            setSrcDirs(srcDirs + listOf("src/main/scala-2.13.${from}+/2.13.${to}"))
//                    }
//                }
//            }
//        }
//    }

    task("dependencyTree") {

        dependsOn("dependencies")
    }

    tasks {
        val jvmTarget = JavaVersion.VERSION_1_8.toString()

        withType<ScalaCompile> {

            targetCompatibility = jvmTarget

            scalaCompileOptions.apply {

//                    isForce = true

                loggingLevel = "verbose"

                val compilerOptions =

                    mutableListOf(
                        "-encoding", "UTF-8",

                        "-deprecation",
                        "-unchecked",
                        "-feature",
                        "-language:higherKinds",
                        "-language:existentials",
                        "-Ywarn-value-discard",
                        "-Ywarn-unused:imports",
                        "-Ywarn-unused:implicits",
                        "-Ywarn-unused:params",
                        "-Ywarn-unused:patvars",
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

            reports.junitXml.required.set(true)
            reports.junitXml.outputLocation.set(File("build/reports/tests/test-xml"))
        }
    }

    java {
        withSourcesJar()
        withJavadocJar()
    }
}

subprojects {}

idea {

    targetVersion = "2020"

    module {

        excludeDirs = excludeDirs + listOf(
            file(".gradle"),
//            file(".github"),

            file("target"),
//                        file ("out"),

            file(".idea"),
            file(".vscode"),
            file(".bloop"),
            file(".bsp"),
            file(".metals"),
            file(".ammonite"),

            file("logs"),

            file("spike"),
        )

        isDownloadJavadoc = true
        isDownloadSources = true
    }
}
