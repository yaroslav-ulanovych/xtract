package xtract.docs.intro

import xtract.FunSuite
import xtract.docs.`var`.Gender

// doc begin
// # Brief intro example
case class Person(name: String, gender: Gender, age: Int, address: Address, status: Status)
case class Address(country: String, city: String)
sealed trait Status
case class Single() extends Status
case class Married(partner: String) extends Status

import xtract.read
// doc end

class BriefIntroSection extends FunSuite {
  test("brief intro example") {
// doc begin
    val data = Map(
      "name" -> "John",
      "gender" -> "Male",
      "age" -> 42,
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
    person shouldBe Person("John", Gender.Male, 42, Address("USA", "NY"), Married("Mary"))
// doc end
  }
}