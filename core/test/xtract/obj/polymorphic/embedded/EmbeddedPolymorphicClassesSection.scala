package xtract.obj.polymorphic.embedded

import xtract._


class Person extends Obj {
  val name = string
  val status = embedded[Status]
}

sealed trait Status extends AbstractObj

class Married extends Status {
  val partner = string
}



class EmbeddedPolymorphicClassesSection extends FunSpec {
//  val layout3perfect = Map(
//    "name" -> "John",
//    "status" -> Map(
//      "type" -> "Married",
//      "args" -> Map(
//        "partner" -> "Mary"
//      )
//    )
//  )

  describe("nested layout + below type hint") {
    val data = Map(
      "name" -> "John",
      "status" -> Map(
        "type" -> "Married",
        "partner" -> "Mary"
      )
    )
    check(data, DefaultReadParams, DefaultWriteParams)
  }

  describe("flat layout + below type hint") {
    val data = Map(
      "name" -> "John",
      "status/type" -> "Married",
      "statusMarried/partner" -> "Mary"
    )
    val layout = FlatLayout("/")
    check(data, DefaultReadParams + layout, DefaultWriteParams + layout)
  }

  def check[T, U](data: T, rp: ReadParams[T], wp: WriteParams[U]) {
    val person = read[Person].from(data)(rp)

    it("read") {
      person.name() shouldBe "John"
      person.status().asInstanceOf[Married].partner() shouldBe "Mary"
    }

    it("write") {
      val data2 = person.write(wp)
      data2 shouldBe data
    }
  }
}
