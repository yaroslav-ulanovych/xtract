package xtract.test.obj.embedded.polymorphic

import xtract._

trait Figure extends AbstractObj

class Rectangle extends Figure {
  val width = int
  val height = int
}

class Circle extends Figure {
  val radius = int
}

class Holder extends Obj {
  val figure = embedded[Figure]
}

class EmbeddedPolymorphicObjectsTest extends FunSuite {
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
    val rectangle = holder.figure().asInstanceOf[Rectangle]
    rectangle.width() shouldBe 4
    rectangle.height() shouldBe 2
  }

  test("read from flat layout") {
    val data = Map(
      "figure/type" -> "Rectangle",
      "figure/rectangle/width" -> 4,
      "figure/rectangle/height" -> 2
    )

    val holder = read[Holder].from(data)(DefaultReadParams + FlatLayout("/"))
    val rectangle = holder.figure().asInstanceOf[Rectangle]
    rectangle.width() shouldBe 4
    rectangle.height() shouldBe 2
  }
}
