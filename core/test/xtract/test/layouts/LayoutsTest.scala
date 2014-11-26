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

  val nested = NestedDiver
  val flat = FlatDiver(".")
  
  


  example(true, nested, fl.simple, thls.near,
    Map(
     "figureType" -> "Rectangle",
      "figure" -> Map(
        "width" -> 4,
        "height" -> 2
      )
    )
  )
  example(true, nested, fl.simple, thls.below,
    Map(
      "figure" -> Map(
        "type" -> "Rectangle",
        "width" -> 4,
        "height" -> 2
      )
    )
  )

  example(true, nested, fl.typeHint, thls.near,
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

  example(true, nested, fl.typeHint, thls.below,
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

  example(true, nested, fl.dedicated, thls.near,
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

  example(true, nested, fl.dedicated, thls.below,
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

  example(true, flat, fl.simple, thls.near,
    Map(
     "figureType" -> "Rectangle",
      "figure.width" -> 4,
      "figure.height" -> 2
    )
  )

  example(true, flat, fl.simple, thls.below,
    Map(
      "figure.type" -> "Rectangle",
      "figure.width" -> 4,
      "figure.height" -> 2
    )
  )

  example(true, flat, fl.typeHint, thls.near,
    Map(
      "figureType" -> "Rectangle",
      "figure.rectangle.width" -> 4,
      "figure.rectangle.height" -> 2
    )
  )

  example(true, flat, fl.typeHint, thls.below,
    Map(
      "figure.type" -> "Rectangle",
      "figure.rectangle.width" -> 4,
      "figure.rectangle.height" -> 2
    )
  )

  example(true, flat, fl.dedicated, thls.near,
    Map(
      "figureType" -> "Rectangle",
      "figure.args.width" -> 4,
      "figure.args.height" -> 2
    )
  )

  example(true, flat, fl.dedicated, thls.below,
    Map(
      "figure.type" -> "Rectangle",
      "figure.args.width" -> 4,
      "figure.args.height" -> 2
    )
  )

  example(false, nested, fl.simple, thls.near,
    Map(
     "type" -> "Rectangle",
      "width" -> 4,
      "height" -> 2
    )
  )

  example(false, nested, fl.simple, thls.below,
    Map(
      "type" -> "Rectangle",
      "width" -> 4,
      "height" -> 2
    )
  )

  example(false, nested, fl.typeHint, thls.near,
    Map(
     "type" -> "Rectangle",
     "rectangle" -> Map(
       "width" -> 4,
       "height" -> 2
     )
    )
  )

  example(false, nested, fl.typeHint, thls.below,
    Map(
      "type" -> "Rectangle",
      "rectangle" -> Map(
        "width" -> 4,
        "height" -> 2
      )
    )
  )

  example(false, nested, fl.dedicated, thls.near,
    Map(
     "type" -> "Rectangle",
      "args" -> Map(
        "width" -> 4,
        "height" -> 2
      )
    )
  )

  example(false, nested, fl.dedicated, thls.below,
    Map(
      "type" -> "Rectangle",
      "args" -> Map(
        "width" -> 4,
        "height" -> 2
      )
    )
  )

  example(false, flat, fl.simple, thls.near,
    Map(
      "type" -> "Rectangle",
      "width" -> 4,
      "height" -> 2
    )
  )

  example(false, flat, fl.simple, thls.below,
    Map(
      "type" -> "Rectangle",
      "width" -> 4,
      "height" -> 2
    )
  )

  example(false, flat, fl.typeHint, thls.near,
    Map(
      "type" -> "Rectangle",
      "rectangle.width" -> 4,
      "rectangle.height" -> 2
    )
  )

  example(false, flat, fl.typeHint, thls.below,
    Map(
      "type" -> "Rectangle",
      "rectangle.width" -> 4,
      "rectangle.height" -> 2
    )
  )

  example(false, flat, fl.dedicated, thls.near,
    Map(
      "type" -> "Rectangle",
      "args.width" -> 4,
      "args.height" -> 2
    )
  )

  example(false, flat, fl.dedicated, thls.below,
    Map(
      "type" -> "Rectangle",
      "args.width" -> 4,
      "args.height" -> 2
    )
  )

  def example(embedded: Boolean, diver: Diver, fl: FieldsLocation, thls: TypeHintLocationStrategy, data: Map[String, Any]) {
    val testName = s"${if (embedded) "embedded" else "standalone"}, ${diver.getClass.getSimpleName}, ${fl.getClass.getSimpleName}, ${thls.getClass.getSimpleName}"

    implicit val readParams = DefaultReadParams + diver + fl + thls
    implicit val writeParams = DefaultWriteParams + diver + fl + thls

    if (embedded) {
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
    } else {
      test(testName + ", obj, read") {
        import obj._
        val figure = read[Figure] from data
        val rectangle = figure.asInstanceOf[Rectangle]
        rectangle.width() shouldBe 4
        rectangle.height() shouldBe 2
      }

      test(testName + ", case class, read") {
        import cc._
        val figure = read[Figure] from data
        val rectangle = figure.asInstanceOf[Rectangle]
        rectangle.width shouldBe 4
        rectangle.height shouldBe 2
      }

      test(testName + ", obj, write") {
        import obj._
        val rectangle = new Rectangle
        rectangle.width := 4
        rectangle.height := 2
        write(rectangle, writeParams) shouldBe data
      }
    }
  }

}
