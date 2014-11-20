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

    val holder = read[Holder].from(data)(DefaultReadParams + FlatLayoutOld("/"))
    val rectangle = holder.figure().asInstanceOf[Rectangle]
    rectangle.width() shouldBe 4
    rectangle.height() shouldBe 2
  }

  test("write nested") {
    val rectangle = new Rectangle
    rectangle.width := 4
    rectangle.height := 2
    val holder = new Holder
    holder.figure := rectangle
    val map = write(holder)

    map shouldBe Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "width" -> 4,
        "height" -> 2
      )
    )
  }

  test("write to flat layout") {
    val rectangle = new Rectangle
    rectangle.width := 4
    rectangle.height := 2
    val holder = new Holder
    holder.figure := rectangle
    val map = write(holder, DefaultWriteParams + FlatDiver(".") + TypeHintFieldsLayout)

    map shouldBe Map(
      "figure.type" -> "Rectangle",
      "figure.rectangle.width" -> 4,
      "figure.rectangle.height" -> 2
    )
  }
}
