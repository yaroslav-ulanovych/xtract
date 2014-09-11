package xtract

class UtilsTest extends FunSuite {
  test("splitFieldNameIntoParts") {
    Utils.splitFieldNameIntoParts("favouriteDogName") shouldBe List("favourite", "Dog", "Name")
  }
}
