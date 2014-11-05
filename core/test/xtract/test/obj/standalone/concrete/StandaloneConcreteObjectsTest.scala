package xtract.test.obj.standalone.concrete

import xtract._

class Settings extends Obj {
  val host = string
  val port = int
}

class StandaloneConcreteObjectsTest extends FunSuite {

  test("read") {
    val settings = read[Settings] from Map("host" -> "localhost", "port" -> 8080)
    settings.host() shouldBe "localhost"
    settings.port() shouldBe 8080
  }

  test("bad field value exception") {
    intercept[BadFieldValueException] {
      read[Settings] from Map("host" -> "localhost", "port" -> "8080")
    }
  }

//  test("converters") {
//    val converter = Converter[String, Int](x => StringUtils.parseInt(x), _.toString)
//    implicit val params = DefaultReadParams + converter
//    read[Settings] from Map("host" -> "localhost", "port" -> "8080")
//  }


  //  test("write to nested layout") {
//    val rect = new Settings
//    rect.width := 4
//    rect.height := 2
//    write(rect) shouldBe Map(
//      "width" -> 4,
//      "height" -> 2
//    )
//  }
//
//  test("write to flat layout") {
//    val rect = new Settings
//    rect.width := 4
//    rect.height := 2
//    write(rect, DefaultWriteParams + FlatLayout("")) shouldBe Map(
//      "width" -> 4,
//      "height" -> 2
//    )
//  }
//
//  test("bad field value exception with converter") {
//    val converter = Converter[String, Int](x => None, _.toString)
//
//    implicit val params = DefaultReadParams + converter
//
//    val e = intercept[BadFieldValueException] {
//      xtract.read[Settings] from Map("width" -> 4, "height" -> "string")
//    }
//  }
}
