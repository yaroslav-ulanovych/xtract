package xtract

class AbstractClassNameTest extends FunSuite {
  test("class") {
    class Status extends AbstractObj
    class Married extends Status
    (new Married).abstractClass shouldBe classOf[Status]
  }

  test("trait") {
    trait Status extends AbstractObj
    class Married extends Status
    (new Married).abstractClass shouldBe classOf[Status]
  }
}
