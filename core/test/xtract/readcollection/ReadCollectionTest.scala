package xtract.readcollection

import xtract.{DefaultReadParams, read, FunSuite}

case class StringListHolder(xs: List[String])

class ReadCollectionTest extends FunSuite {
  test("test") {
    val res = read.readCollection(classOf[List[_]], classOf[String], List("a", "b"), DefaultReadParams)
    res shouldBe List("a", "b")
  }

  test("bug List[Int] is read to List[String] should be fixed") {
    intercept[Exception] {
      read[StringListHolder].from(Map("xs" -> List(1, 2)))
    }
  }
}
