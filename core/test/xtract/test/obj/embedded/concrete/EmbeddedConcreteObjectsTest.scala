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
  test("write to nested layout") {
    val person = new Person
    val address = new Address
    address.country := "USA"
    address.city := "NY"
    person.address := address
    write(person) shouldBe Map(
      "address" -> Map(
        "country" -> "USA",
        "city" -> "NY"
      )
    )
  }

  test("write to flat layout") {
    val person = new Person
    val address = new Address
    address.country := "USA"
    address.city := "NY"
    person.address := address

    write(person, DefaultWriteParams + FlatLayout("/")) shouldBe Map(
      "address/country" -> "USA",
      "address/city" -> "NY"
    )
  }
}
