package xtract.test.cc.standalone.concrete

import xtract.{read, FunSuite}

case class Settings(host: String, port: Int)

class StandaloneConcreteCaseClassesTest extends FunSuite {
  test("read") {
    val settings = read[Settings] from Map("host" -> "localhost", "port" -> 8080)
    settings shouldBe Settings("localhost", 8080)
  }
}
