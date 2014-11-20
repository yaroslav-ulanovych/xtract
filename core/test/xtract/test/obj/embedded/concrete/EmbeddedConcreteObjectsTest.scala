package xtract.test.obj.embedded.concrete

import xtract._

class Person extends Obj {
  val address = embedded[Address]
}

class Address extends Obj {
  val country = string
  val city = string
}


class EmbeddedConcreteObjectsTest extends FunSuite {
  test("read nested") {
    val data = Map(
      "address" -> Map(
        "country" -> "USA",
        "city" -> "NY"
      )
    )

    val person = read[Person] from data

    person.address().country() shouldBe "USA"
    person.address().city() shouldBe "NY"
  }


  test("read flat") {
    val data = Map(
      "address.country" -> "USA",
      "address.city" -> "NY"
    )

    val person = read[Person].from(data)(DefaultReadParams + FlatDiver("."))

    person.address().country() shouldBe "USA"
    person.address().city() shouldBe "NY"
  }

  test("write nested") {
    val person = new Person

    person.address := new Address

    person.address().country := "USA"
    person.address().city := "NY"

    write(person) shouldBe Map(
      "address" -> Map(
        "country" -> "USA",
        "city" -> "NY"
      )
    )
  }

  test("write flat") {
    val person = new Person
    person.address := new Address
    person.address().country := "USA"
    person.address().city := "NY"

    write(person, DefaultWriteParams + FlatDiver(".")) shouldBe Map(
      "address.country" -> "USA",
      "address.city" -> "NY"
    )
  }
}
