package xtract

class LayoutsTest extends FunSuite {
  test("nested layout") {
    val data = Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "args" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
    )

    val Some(Right((data2, layout2))) = NestedLayoutOld.dive1(data, "figure", DefaultReadParams)

    data2 shouldBe data("figure")
    layout2 shouldBe NestedLayoutOld

    val Some(Right((data3, layout3))) = NestedLayoutOld.dive2(data2, "Rectangle", DefaultReadParams)

    data3 shouldBe data("figure")("args")
    layout3 shouldBe NestedLayoutOld
  }

  test("flat layout") {
    val data = Map(
      "figure/type" -> "Rectangle",
      "figure/rectangle/width" -> 4,
      "figure/rectangle/height" -> 2
    )

    val Some(Right((data2, layout2))) = FlatLayoutOld("/").dive1(data, "figure", DefaultReadParams)

    data2 shouldBe data
    layout2 shouldBe FlatLayoutOld("/", "figure/")

    val Some(Right((data3, layout3))) = layout2.dive2(data2, "rectangle", DefaultReadParams)

    data3 shouldBe data
    layout3 shouldBe FlatLayoutOld("/", "figure/rectangle/")
  }
}
