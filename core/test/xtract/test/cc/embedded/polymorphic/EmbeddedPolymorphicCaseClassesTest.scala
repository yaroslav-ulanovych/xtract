package xtract.test.cc.embedded.polymorphic

import xtract.{FlatLayout, DefaultReadParams, read, FunSuite}

trait Figure

case class Circle(radius: Int) extends Figure

case class Rectangle(width: Int, height: Int) extends Figure

case class Holder(figure: Figure)

class EmbeddedPolymorphicCaseClassesTest extends FunSuite {
  test("read from nested layout") {
    val data = Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "args" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
    )

    val holder = read[Holder] from data
    val rectangle = holder.figure.asInstanceOf[Rectangle]
    rectangle.width shouldBe 4
    rectangle.height shouldBe 2
  }

  test("read from flat layout") {
    val data = Map(
      "figure/type" -> "Rectangle",
      "figure/rectangle/width" -> 4,
      "figure/rectangle/height" -> 2
    )

    val holder = read[Holder].from(data)(DefaultReadParams + FlatLayout("/"))
    val rectangle = holder.figure.asInstanceOf[Rectangle]
    rectangle.width shouldBe 4
    rectangle.height shouldBe 2
  }
}
