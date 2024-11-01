package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

class MagnetSpec extends BaseSpec {

  describe(Magnet.OptionMagnetCap.toString) {

//    it("mixin & pattern match ") {
//TODO: doesn't work unless scala 3
//      trait Mixin
//
//      val k: Option[Int] & Mixin = Some(1).asInstanceOf[Option[Int] & Mixin]
//
//      k match {
//        case Some(1) =>
//      }
//    }

    it("some") {

      val v: Option[String] = Some("a")
      val v1: Magnet.OptionMagnet[String] = v
      val v2: Magnet.OptionMagnet[String] = "a"

      assert(Seq(v, v1, v2).distinct.size == 1)

      v1.revoke match {
        case Some("a") =>
      }
    }

    it("none") {

      val v: Option[String] = None
      val v1: Magnet.OptionMagnet[String] = v
      val s: String = null
      val v2: Magnet.OptionMagnet[String] = s

      assert(Seq(v, v1, v2).distinct.size == 1)

      v1.revoke match {
        case None =>
      }
    }
  }

  describe(Magnet.PreferRightCap.toString) {

    it("left") {

      val v: Either[String, Int] = Left("a")
      val v1: Magnet.PreferRightMagnet[String, Int] = v
      val v2: Magnet.PreferRightMagnet[String, Int] = "a"

      assert(Seq(v, v1, v2).distinct.size == 1)

      v1.revoke match {
        case Left(v) => v
      }
    }

    it("right") {

      val v: Either[String, Int] = Right(1)
      val v1: Magnet.PreferRightMagnet[String, Int] = v
      val v2: Magnet.PreferRightMagnet[String, Int] = 1

      assert(Seq(v, v1, v2).distinct.size == 1)

      v1.revoke match {
        case Right(v) => v
      }
    }
  }
}
