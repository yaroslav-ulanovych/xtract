package xtract.obj.polymorphic.standalone

import xtract._

class Holder extends Obj {
  val figure = embedded[Figure]
}

trait Figure extends AbstractObj

class Rectangle extends Figure {
  val width = int
  val height = int
}

class Circle extends Figure {
  val radius = int
}

class StandalonePolymorphicObjectsTest extends FunSuite {
  test("read standalone polymorphic object from nested layout") {
    val data = Map(
      "type" -> "Rectangle",
      "width" -> 4,
      "height" -> 2
    )

    val figure = read[Figure] from data
    figure.asInstanceOf[Rectangle].width() shouldBe 4
    figure.asInstanceOf[Rectangle].height() shouldBe 2
  }

  test("read embedded polymorphic object from nested layout") {
    val data = Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "width" -> 4,
        "height" -> 2
      )
    )

    val holder = read[Holder] from data
    holder.figure().asInstanceOf[Rectangle].width() shouldBe 4
    holder.figure().asInstanceOf[Rectangle].height() shouldBe 2
  }

  test("read standalone polymorphic from flat layout") {
    val data = Map(
      "type" -> "Rectangle",
      "rectangle/width" -> 4,
      "rectangle/height" -> 2
    )
    implicit val params = DefaultReadParams + FlatLayout("/")
    val figure = read[Figure] from data
    figure.asInstanceOf[Rectangle].width() shouldBe 4
    figure.asInstanceOf[Rectangle].height() shouldBe 2
  }
}
