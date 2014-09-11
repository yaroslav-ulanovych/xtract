package xtract.docs.embedded.polymorphic

import xtract.{read, FunSuite}

// doc begin
// # <a name="embedded-polymorphic-classes-section">Embedded polymorphic classes</a>
// Case classes may contain fields of polymorphic types, e.g. those that can hold instances of
// several different case classes (that extend that polymorphic types).
case class Person(name: String, status: Status)
trait Status
case class Single() extends Status
case class Married(partner: String) extends Status
// The task is like [previous case](#embedded-polymorphic-classes-section),
// but complicated in that, that we don't know the concrete type of data being read.
// Thus data should contain some hint, using which we should get the concrete type to read.
// doc end
class EmbeddedPolymorphicClassesSection extends FunSuite {
// doc end
// There are few reasonable ways where to put that hint.
// We can put it right between other fields
  val layout1 = Map(
    "name" -> "John",
    "status" -> Map(
      "type" -> "Married",
      "partner" -> "Mary"
    )
  )
// or one level above
  val layout2 = Map(
    "name" -> "John",
    "statusType" -> "Married",
    "status" -> Map(
      "partner" -> "Mary"
    )
  )
// To avoid small risk of name clashes
  val layout3perfect = Map(
    "name" -> "John",
    "status" -> Map(
      "type" -> "Married",
      "args" -> Map(
        "partner" -> "Mary"
      )
    )
  )

// And the flat variant
  val layout4flat = Map(
    "name" -> "John",
    "statusType" -> "Married",
    "status_married_partner" -> "Mary"
  )

//  test("layout 1") {
//    val person = read[Person] from layout1
//    person shouldBe Person("John", Married("Mary"))
//  }


  test("layout 1") {
    val person = read[Person] from layout1
    person shouldBe Person("John", Married("Mary"))
  }
}
