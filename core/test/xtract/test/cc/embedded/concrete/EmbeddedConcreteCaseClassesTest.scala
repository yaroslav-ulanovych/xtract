package xtract.test.cc.embedded.concrete

import xtract._

case class Person(address: Address)

case class Address(country: String, city: String)

class EmbeddedConcreteCaseClassesTest extends FunSuite {
  test("read nested") {
    val data = Map(
      "address" -> Map(
        "country" -> "USA",
        "city" -> "NY"
      )
    )

    val person = read[Person] from data

    person shouldBe Person(Address("USA", "NY"))
  }

  test("read flat") {
    val data = Map(
      "address.country" -> "USA",
      "address.city" -> "NY"
    )

    val person = read[Person].from(data)(DefaultReadParams + FlatDiver("."))

    person shouldBe Person(Address("USA", "NY"))
  }

//  test("write nested") {
//    val person = Person(Address("USA", "NY"))
//    val data = write(person)
//    data shouldBe Map(
//      "address" -> Map(
//        "country" -> "USA",
//        "city" -> "NY"
//      )
//    )
//  }
}
