package xtract

class CasingTest extends FunSuite {
  test("lower camel case") {
    LowerCamelCase.apply(List("HOME", "ADDRESS")) shouldBe List("home", "Address")
  }

  test("lower case") {
    LowerCase.apply(List("HOME", "ADDRESS")) shouldBe List("home", "address")
  }

  test("upper case") {
    UpperCase.apply(List("home", "address")) shouldBe List("HOME", "ADDRESS")
  }
}
