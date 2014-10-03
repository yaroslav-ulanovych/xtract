package xtract.test.misc.writeconverter

import xtract._

class Person extends Obj {
  val gender = field[Gender]
}

class WriteConverterTest extends FunSuite {
  test("test") {
    val person = new Person
    person.gender := Gender.Male
    intercept[Exception] {
      write(person)
    }
  }

  test("test 2") {
    val person = new Person
    person.gender := Gender.Male
    val data = write(person, DefaultWriteParams + StringToJavaEnumConverter)
    data shouldBe Map("gender" -> "Male")
  }

}
