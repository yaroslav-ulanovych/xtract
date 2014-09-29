package xtract

class UtilsTest extends FunSuite {
  test("splitFieldNameIntoParts") {
    Utils.splitFieldNameIntoParts("favouriteDogName") shouldBe List("favourite", "Dog", "Name")
    Utils.splitFieldNameIntoParts("FavouriteDogName") shouldBe List("Favourite", "Dog", "Name")
  }
}
