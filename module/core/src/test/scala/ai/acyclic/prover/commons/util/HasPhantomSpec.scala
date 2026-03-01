//package ai.acyclic.prover.commons.util
//
//import ai.acyclic.prover.commons.testlib.BaseSpec
//import ai.acyclic.prover.commons.verification.Verify
//
//class HasPhantomSpec extends BaseSpec {
//
//  object App extends HasPhantom {
//    trait SubPhantom extends Phantom.Impl
//    trait NotPhantom
//  }
//
//  describe("HasPhantom") {
//
//    it("should allow getting a subtype of Phantom.Impl") {
//      val res = App.Phantom.get[App.SubPhantom]
//      assert(res == null)
//    }
//
//    it("should cause a compilation error if type is not a subtype of Phantom.Impl") {
//      Verify.typeError(
//        "App.Phantom.get[App.NotPhantom]"
//      )
//      Verify.typeError(
//        "val a: App.NotPhantom = App.Phantom.get()"
//      )
//      Verify.typeError(
//        "val a: App.NotPhantom = App.Phantom()"
//      )
//    }
//
//    it("should cause a compilation error if type is Int") {
//      Verify.typeError(
//        "App.Phantom.get[Int]"
//      )
//    }
//  }
//}
