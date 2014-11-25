package xtract.test.layouts

import xtract._

class LayoutsTest extends FunSuite {

  val thls = new {
    val near = NearTypeHintLocationStrategy("type")
    val below = BelowTypeHintLocationStrategy("type")
  }

  val fl = new {
    val simple = SimpleFieldsLocation
    val typeHint = TypeHintFieldsLocation
    val dedicated = DedicatedFieldsLocation("args")
  }


  example(fl.simple, thls.near,
    Map(
     "figureType" -> "Rectangle",
      "figure" -> Map(
        "width" -> 4,
        "height" -> 2
      )
    )
  )
  example(fl.simple, thls.below,
    Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "width" -> 4,
        "height" -> 2
      )
    )
  )

  example(fl.typeHint, thls.near,
    Map(
     "figureType" -> "Rectangle",
      "figure" -> Map(
        "rectangle" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
    )
  )

  example(fl.typeHint, thls.below,
    Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "rectangle" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
    )
  )

  example(fl.dedicated, thls.near,
    Map(
     "figureType" -> "Rectangle",
      "figure" -> Map(
        "args" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
    )
  )

  example(fl.dedicated, thls.below,
    Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "args" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
    )
  )

  def example(fl: FieldsLocation, thls: TypeHintLocationStrategy, data: Map[String, Any]) {
    val testName = s"${fl.getClass.getSimpleName}, ${thls.getClass.getSimpleName}"

    implicit val readParams = DefaultReadParams + fl + thls
    implicit val writeParams = DefaultWriteParams + fl + thls

    test(testName + ", obj, read") {
      import obj._
      val holder = read[Holder] from data
      val figure = holder.figure()
      val rectangle = figure.asInstanceOf[Rectangle]
      rectangle.width() shouldBe 4
      rectangle.height() shouldBe 2
    }

    test(testName + ", case class, read") {
      import cc._
      val holder = read[Holder] from data
      val figure = holder.figure
      val rectangle = figure.asInstanceOf[Rectangle]
      rectangle.width shouldBe 4
      rectangle.height shouldBe 2
    }

    test(testName + ", obj, write") {
      import obj._
      val rectangle = new Rectangle
      rectangle.width := 4
      rectangle.height := 2
      val holder = new Holder
      holder.figure := rectangle
      write(holder, writeParams) shouldBe data
    }

//    test(testName + ", cc, write") {
//      import cc._
//      val holder = Holder(Rectangle(4, 2))
//      write(holder) shouldBe data
//    }
  }

}
