package xtract

class VarTest extends FunSuite {
  test("nested case class companion object bytecode naming") {
    case class Nested()
    classOf[Nested].getName shouldBe "xtract.VarTest$$anonfun$1$Nested$2"
    Nested.getClass.getName shouldBe "xtract.VarTest$$anonfun$1$Nested$3$"
  }
}
