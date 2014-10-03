package xtract.test.misc.primitivetypes

import xtract.{read, Obj, FunSuite}

class Foo extends Obj {
  val a = field[Int]
  val b = field[Int]
  val c = field[Integer]
  val d = field[Integer]
}

class PrimitiveTypesTest extends FunSuite {
  test("read to primitive types") {
    val data = Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)
    read[Foo] from data
  }
}
