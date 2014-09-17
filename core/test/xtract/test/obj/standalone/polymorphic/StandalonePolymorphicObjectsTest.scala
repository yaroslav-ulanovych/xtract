package xtract.test.obj.standalone.polymorphic

import xtract._

trait Figure extends AbstractObj

class Rectangle extends Figure {
  val width = int
  val height = int
}

class Circle extends Figure {
  val radius = int
}

class StandalonePolymorphicObjectsTest extends FunSpec {
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
      figure.asInstanceOf[Rectangle].width() shouldBe 4
      figure.asInstanceOf[Rectangle].height() shouldBe 2
    }

    it("write") {
      val rect = new Rectangle
      rect.width := 4
      rect.height := 2
      write(rect) shouldBe data
    }
  }

  describe("flat layout") {
    val data = Map(
      "type" -> "Rectangle",
      "rectangle/width" -> 4,
      "rectangle/height" -> 2
    )

    it("read") {
      val figure = read[Figure].from(data)(DefaultReadParams + FlatLayout("/"))
      figure.asInstanceOf[Rectangle].width() shouldBe 4
      figure.asInstanceOf[Rectangle].height() shouldBe 2
    }

    it("write") {
      val rect = new Rectangle
      rect.width := 4
      rect.height := 2
      write(rect, DefaultWriteParams + FlatLayout("/")) shouldBe data
    }
  }
}
