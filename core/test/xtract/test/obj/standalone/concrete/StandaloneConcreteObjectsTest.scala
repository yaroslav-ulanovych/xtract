package xtract.test.obj.standalone.concrete

import xtract._

class Rectangle extends Obj {
  val width = int
  val height = int
}

class StandaloneConcreteObjectsTest extends FunSuite {
  test("write to nested layout") {
    val rect = new Rectangle
    rect.width := 4
    rect.height := 2
    write(rect) shouldBe Map(
      "width" -> 4,
      "height" -> 2
    )
  }

  test("write to flat layout") {
    val rect = new Rectangle
    rect.width := 4
    rect.height := 2
    write(rect, DefaultWriteParams + FlatLayout("")) shouldBe Map(
      "width" -> 4,
      "height" -> 2
    )
  }
}
