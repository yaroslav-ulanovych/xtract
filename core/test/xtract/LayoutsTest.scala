package xtract

package cc {
  trait Figure

  case class Rectangle(width: Int, height: Int) extends Figure

  case class Circle(radius: Int) extends Figure

  case class Holder(figure: Figure)
}

package obj {
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
}

class LayoutsTest extends FunSpec {
  sealed trait Embeddedness
  object Embedded extends Embeddedness
  object Standalone extends Embeddedness

  val nearTypeHintLocationStrategy = NearTypeHintLocationStrategy("type")
  val dedicatedFieldsLocation = DedicatedFieldsLocation("args")
  val belowTypeHintLocationStrategy = BelowTypeHintLocationStrategy("type")

  case class Params(diver: Diver, thls: TypeHintLocationStrategy, fl: FieldsLocation)

  describe("embedded") {
    describe("NestedDiver, NearTypeHintLocationStrategy, SimpleFieldsLocation") {
      val params = Params(NestedDiver, nearTypeHintLocationStrategy, SimpleFieldsLocation)
      val data = Map(
        "figureType" -> "Rectangle",
        "figure" -> Map(
          "width" -> 4,
          "height" -> 2
        )
      )
      describe("objects") {
        it("read") {
          performObjReadTest(data, params)
        }
        it("write") {
          performObjWriteTest(data, params)
        }
      }
      describe("case classes") {
        it("read") {
          performCaseClassReadTest(data, params)
        }
        it("write") {
          performCaseClassWriteTest(data, params)
        }
      }
    }

    describe("NestedDiver, NearTypeHintLocationStrategy, TypeHintFieldsLocation") {
      val params = Params(NestedDiver, nearTypeHintLocationStrategy, TypeHintFieldsLocation)
      val data = Map(
        "figureType" -> "Rectangle",
        "figure" -> Map(
          "rectangle" -> Map(
            "width" -> 4,
            "height" -> 2
          )
        )
      )
      describe("objects") {
        it("read") {
          performObjReadTest(data, params)
        }
        it("write") {
          performObjWriteTest(data, params)
        }
      }
      describe("case classes") {
        it("read") {
          performCaseClassReadTest(data, params)
        }
        it("write") {
          performCaseClassWriteTest(data, params)
        }
      }
    }

    describe("NestedDiver, NearTypeHintLocationStrategy, DedicatedFieldsLocation") {
      val params = Params(NestedDiver, nearTypeHintLocationStrategy, dedicatedFieldsLocation)
      val data = Map(
        "figureType" -> "Rectangle",
        "figure" -> Map(
          "args" -> Map(
            "width" -> 4,
            "height" -> 2
          )
        )
      )
      describe("objects") {
        it("read") {
          performObjReadTest(data, params)
        }
        it("write") {
          performObjWriteTest(data, params)
        }
      }
      describe("case classes") {
        it("read") {
          performCaseClassReadTest(data, params)
        }
        it("write") {
          performCaseClassWriteTest(data, params)
        }
      }
    }

    describe("NestedDiver, BelowTypeHintLocationStrategy, SimpleFieldsLocation") {
      val params = Params(NestedDiver, belowTypeHintLocationStrategy, SimpleFieldsLocation)
      val data = Map(
        "figure" -> Map(
          "type" -> "Rectangle",
          "width" -> 4,
          "height" -> 2
        )
      )
      describe("objects") {
        it("read") {
          performObjReadTest(data, params)
        }
        it("write") {
          performObjWriteTest(data, params)
        }
      }
      describe("case classes") {
        it("read") {
          performCaseClassReadTest(data, params)
        }
        it("write") {
          performCaseClassWriteTest(data, params)
        }
      }
    }

    describe("NestedDiver, BelowTypeHintLocationStrategy, TypeHintFieldsLocation") {
      val params = Params(NestedDiver, belowTypeHintLocationStrategy, TypeHintFieldsLocation)
      val data = Map(
        "figure" -> Map(
          "type" -> "Rectangle",
          "rectangle" -> Map(
            "width" -> 4,
            "height" -> 2
          )
        )
      )
      describe("objects") {
        it("read") {
          performObjReadTest(data, params)
        }
        it("write") {
          performObjWriteTest(data, params)
        }
      }
      describe("case classes") {
        it("read") {
          performCaseClassReadTest(data, params)
        }
        it("write") {
          performCaseClassWriteTest(data, params)
        }
      }
    }

    describe("NestedDiver, BelowTypeHintLocationStrategy, DedicatedFieldsLocation") {
      val params = Params(NestedDiver, belowTypeHintLocationStrategy, dedicatedFieldsLocation)
      val data = Map(
        "figure" -> Map(
          "type" -> "Rectangle",
          "args" -> Map(
            "width" -> 4,
            "height" -> 2
          )
        )
      )
      describe("objects") {
        it("read") {
          performObjReadTest(data, params)
        }
        it("write") {
          performObjWriteTest(data, params)
        }
      }
      describe("case classes") {
        it("read") {
          performCaseClassReadTest(data, params)
        }
        it("write") {
          performCaseClassWriteTest(data, params)
        }
      }
    }

    def performObjReadTest(data: Map[String, Any], params: Params) {
      import obj._
      implicit val readParams = DefaultReadParams + params.diver + params.thls + params.fl
      val holder = read[Holder] from data
      val figure = holder.figure()
      val rectangle = figure.asInstanceOf[Rectangle]
      rectangle.width() shouldBe 4
      rectangle.height() shouldBe 2
    }

    def performCaseClassReadTest(data: Map[String, Any], params: Params) {
      import cc._
      implicit val readParams = DefaultReadParams + params.diver + params.thls + params.fl
      val holder = read[Holder] from data
      val figure = holder.figure
      val rectangle = figure.asInstanceOf[Rectangle]
      rectangle.width shouldBe 4
      rectangle.height shouldBe 2
    }

    def performObjWriteTest(data: Map[String, Any], params: Params) {
      ???
    }

    def performCaseClassWriteTest(data: Map[String, Any], params: Params) {
      ???
    }
  }


}
