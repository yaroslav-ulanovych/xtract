package xtract.obj.polymorphic.standalone

import xtract.{NearTypeHintLocation, DefaultWriteParams, AbstractObj, FunSuite}

trait Figure extends AbstractObj

class Rectangle extends Figure {
  val width = int
  val height = int
}

class Circle extends Figure {
  val radius = int
}

class StandalonePolymorphicObjectsTest extends FunSuite {
  val layout1 = Map(
    "type" -> "Rectangle",
    "width" -> 4,
    "height" -> 2
  )

  val layout2 = Map(
  "type" -> "Rectangle",
    "args" -> Map(
    "width" -> 4,
    "height" -> 2
    )
  )

  val layout3 = Map(
    "type" -> "Rectangle",
    "rectangleWidth" -> 4,
    "rectangleHeight" -> 2
  )

  test("write") {
    val figure = new Rectangle
    figure.width := 4
    figure.height := 2

    val data = figure.write(DefaultWriteParams + NearTypeHintLocation("type"))
    println(data)
  }
}
