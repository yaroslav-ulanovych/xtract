package xtract.test.cc.standalone.polymorphic

import xtract._

trait Figure

case class Rectangle(width: Int, height: Int) extends Figure

case class Circle(radius: Int) extends Figure

class StandalonePolymorphicCaseClassesTest extends FunSpec {
  describe("nested layout") {
    val data = Map(
      "type" -> "Rectangle",
      "args" -> Map(
        "width" -> 4,
        "height" -> 2
      )
    )

    it("read") {
      val figure = read[Figure] from data
      figure shouldBe Rectangle(4, 2)
    }

//    it("write") {
//      val rect = Rectangle(4, 2)
//      write(rect) shouldBe data
//    }
  }

  describe("flat layout") {
    val data = Map(
      "type" -> "Rectangle",
      "rectangle/width" -> 4,
      "rectangle/height" -> 2
    )

    it("read") {
      val figure = read[Figure].from(data)(DefaultReadParams + FlatLayoutOld("/"))
      figure shouldBe Rectangle(4, 2)
    }

//    it("write") {
//      val rect = Rectangle(4, 2)
//      write(rect, DefaultWriteParams + FlatLayout("/")) shouldBe data
//    }
  }

}
