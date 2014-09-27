package xtract.test.cc.standalone.concrete

import xtract.{read, FunSuite}

case class Settings(host: String, port: Int)

case class A(xs: List[String])

class StandaloneConcreteCaseClassesTest extends FunSuite {
  test("read") {
    val settings = read[Settings] from Map("host" -> "localhost", "port" -> 8080)
    settings shouldBe Settings("localhost", 8080)
  }

  test("lists") {
    val a = read[A] from Map("xs" -> Vector("a", "b"))
    a shouldBe A(List("a", "b"))
  }
}
