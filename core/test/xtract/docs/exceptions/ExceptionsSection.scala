package xtract.docs.exceptions

import xtract.{FunSuite, read}

case class Person(id: Int, name: String)


// doc begin
// # <a name="exceptions-section">Exceptions</a>
// When parsing fails an exception is thrown.
// They (exceptions) tend to contain as much information as possible,
// and all those information is available not only from `getMessage` method,
// but via dedicated fields. That allows you to create meaningful
// error messages for users of your api, for instance.
// doc end
class ExceptionsSection extends FunSuite {
// doc begin
// ## <a name="missing-field-exception-section">Missing field exception</a>
  import xtract.MissingFieldException
// doc end
  test("MissingFieldException") {
// doc begin
// Missing field exception
    val data = Map[String, Any]()
    val e = intercept[MissingFieldException] {
      read[Person] from data
    }
// doc begin
// gives you information about class we were trying to instantiate,
// missing field name and data we were looking for the field in
    e.getMessage shouldBe "missing field Person.id in Map()"
    e.klass shouldBe classOf[Person]
    e.field shouldBe "id"
    e.data shouldBe data
// doc end
  }

// doc begin
// ## <a name="bad-field-value-exception-section">Bad field value exception</a>
  import xtract.BadFieldValueException
// doc end

  test("BadFieldValueException") {
// doc begin
// Bad field value exception is thrown, when your data source contains a value of type you don't expect,
// like bool instead of int in example below
    val data = Map[String, Any]("id" -> false, "name" -> "John")
// You also get a detailed exception
    val e = intercept[BadFieldValueException] {
      read[Person] from data
    }
    e.getMessage shouldBe "bad value for Person.id field of int type: Boolean(false), converter: None"
    e.klass shouldBe classOf[Person]
    e.field shouldBe "id"
    e.fieldType shouldBe classOf[Int]
    e.value shouldBe false
    e.valueType shouldBe classOf[java.lang.Boolean]
// doc end
  }

// doc begin
// ## <a name="not-companion-object-exception-section">Not companion object exception</a>
  import xtract.NotCompanionObjectException
// doc end
  test("NotCompanionObjectException") {
// doc begin
// One of the reasons of this exception is reading a nested case class.
    def enclosingMethod = {
      case class Nested(id: Int, name: String)
      val e = intercept[NotCompanionObjectException] {
        read[Nested] from Map[String, Any]()
      }
// You'll get an exception with a message about MODULE$ field
      e.reason shouldBe "it has no MODULE$ field"
    }
    enclosingMethod
// That happens because to instantiate a case class read function
// searches for a companion object. For top level case classes it lays
// in static field MODULE$ in companion object class, but that's not true
// for nested ones, to fix that you have to
// [supply companion object explicitly](#instantiators-section).
// doc end
  }
}