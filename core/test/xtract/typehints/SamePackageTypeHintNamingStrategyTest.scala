package xtract.typehints

import xtract.{SamePackageTypeHintNamingStrategy, FunSuite}

class SamePackageTypeHintNamingStrategyTest extends FunSuite {
  test("test") {
    SamePackageTypeHintNamingStrategy.getTypeHint(new Married) shouldBe List("Married")
  }
}