package ai.acyclic.prover.commons.meta

import ai.acyclic.prover.commons.testlib.BaseSpec

class RuntimeTagRelaySpec extends BaseSpec {

  import RuntimeTagRelaySpec._
  import scala.reflect.runtime.universe

  val subject = RuntimeTagRelay.Canonical

  it("alias") {

    val tTag = implicitly[universe.TypeTag[AA]]

    subject
      .classFromType(tTag)
      .toString()
      .shouldBe(
        "java.io.Serializable"
      )
  }

  it("generic") {

    val tTag = implicitly[universe.TypeTag[List[Int]]]

    subject
      .classFromType(tTag)
      .toString()
      .shouldBe(
        "scala.collection.immutable.List"
      )
  }

  it("dependent") {

    val tTag = implicitly[universe.TypeTag[universe.TypeTag[Int]]]

    subject
      .classFromType(tTag)
      .toString()
      .shouldBe(
        "scala.reflect.api.TypeTags$TypeTag"
      )
  }

}

object RuntimeTagRelaySpec {

  trait Example

  type AA = Serializable

}
