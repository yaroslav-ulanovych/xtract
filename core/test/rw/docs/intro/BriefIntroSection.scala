package rw.docs.intro

import rw.FunSuite
import xtract.{read, Obj, AbstractObj}


class Person extends Obj {
  val name = string
  val gender = custom[Gender, String](_.name(), s => Gender.values().find(_.name() == s))
  val address = embedded[Address]
  val status = embedded[Status]
}

class Address extends Obj {
  val country = string
  val city = string
}

trait Status extends AbstractObj

class Single extends Status

class Married extends Status {
  val partner = string
}

class BriefIntroSection extends FunSuite {
  test("1") {
    val data = Map(
      "name" -> "John",
      "gender" -> "Male",
      "address" -> Map(
        "country" -> "USA",
        "city" -> "NY"
      ),
      "status" -> Map(
        "type" -> "Married",
        "partner" -> "Mary"
      )
    )
    val person = read[Person] from data
    person.name() shouldBe "John"
    person.gender() shouldBe Gender.Male
    person.address().country() shouldBe "USA"
    person.address().city() shouldBe "NY"
    person.status().asInstanceOf[Married].partner() shouldBe "Mary"

    val data2 = person.write()
  }
}
