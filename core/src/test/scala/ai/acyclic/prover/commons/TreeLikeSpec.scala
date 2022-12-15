package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.testlib.BaseSpec

abstract class TreeLikeSpec extends BaseSpec {

  lazy val format: TreeFormat = TreeFormat.Indent2

  lazy val formatFixtures = TreeFormatFixtures(format)

  lazy val Demo = formatFixtures.Demo

  val tree1 = Demo(
    "aaa",
    Seq(
      Demo(
        "bbb",
        Seq(
          Demo("ddd")
        )
      ),
      Demo(
        "ccc"
      )
    )
  )

  val tree2 = Demo(
    "aaa\n%%%%%",
    Seq(
      Demo(
        "bbb\n%%%%%",
        Seq(
          Demo("ddd\n%%%%%")
        )
      ),
      Demo(
        "ccc\n%%%%%"
      )
    )
  )
}
