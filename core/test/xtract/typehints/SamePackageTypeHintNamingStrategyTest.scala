package xtract.typehints

import xtract.{FieldName, SamePackageTypeHintNamingStrategy, FunSuite}

class SamePackageTypeHintNamingStrategyTest extends FunSuite {
  test("test") {
    SamePackageTypeHintNamingStrategy.getTypeHint(new Married) shouldBe FieldName(List("Married"))
  }
}
