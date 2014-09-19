package xtract.test.fqname

import xtract.{SamePackageTypeHintNamingStrategy, Obj, AbstractObj, FunSuite}

trait Figure extends AbstractObj

class Rectangle extends Figure {
  val width = int
  val height = int
}

class Circle extends Figure {
  val radius = int
}

class Holder extends Obj {
  val figure = embedded[Figure]
}

class FQNameTest extends FunSuite{
  test("fqname") {
    val rect = new Rectangle
    println(rect.width.fqname(SamePackageTypeHintNamingStrategy))
  }
}
