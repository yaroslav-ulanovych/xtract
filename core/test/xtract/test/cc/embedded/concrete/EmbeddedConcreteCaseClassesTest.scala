package xtract.test.cc.embedded.concrete

import xtract.{read, FunSuite}

case class Settings(web: WebServerSettings)

case class WebServerSettings(host: String, port: Int)

class EmbeddedConcreteCaseClassesTest extends FunSuite {
  test("foo") {
    read[Settings] from Map("web" -> Map("host" -> "localhost", "port" -> 8080))
  }
}
