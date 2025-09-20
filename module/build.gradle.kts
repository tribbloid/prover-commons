val vs = versions()

allprojects {
    group = "ai.acyclic.prover-commons"

    dependencies {

        scalaCompilerPlugins("org.typelevel:kind-projector_${vs.scala.v}:0.13.3")
    }

    tasks {

        withType<ScalaCompile> {

            scalaCompileOptions.apply {

                additionalParameters.addAll(
                    listOf(
//                        "-Xsource:3",
                        "-Xsource:3-cross", // maximally similar to Scala 3
                        // quickfix should be disabled ASAP after migration
//                        "-quickfix:any",
//                        "-quickfix:cat=scala3-migration",

                        // the above "quickfix" can't handle many syntax changes, like [_] => [?] or import x._ => import x.*
//                      // in this case IntelliJ IDEA analyze/`Run inspection by name`/quickfix should be used

//                        "-quickfix:help", TODO: this doesn't work
//                        "-rewrite",
//                        "-quickfix:cat=scala3-migration"

                        "-Wconf:msg=lambda-parens:s",
                        "-Xsource-features:case-apply-copy-access",// this is the standard for Scala 3

                        "-P:kind-projector:underscore-placeholders"
                    )
                )
            }
        }
    }
}
