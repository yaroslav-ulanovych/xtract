package rw.docs.embedded.concrete

import xtract._


class Person extends Obj {
  val address = embedded[Address]
}

class Address extends Obj {
  val country = string
  val city = string
}


class EmbeddedConcreteClassesSection extends FunSuite {
  

  val data = Map(
    "name" -> "John",
    "address_country" -> "USA",
    "address_city" -> "NY"
  )

  test("nested layout") {
    val data = Map(
      "name" -> "John",
      "address" -> Map(
        "country" -> "USA",
        "city" -> "NY"
      )
    )
    val person = read[Person] from data
  }

  test("flat layout") {
    implicit val params = DefaultReadParams + FlatLayout("_")
    val person = read[Person] from data
  }
}
